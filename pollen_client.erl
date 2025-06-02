%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(pollen_client).

-export([main/1]).
-export([ping/0,ch_list/0]).

%% Main function called at the run
main(Args) ->
    start_client(Args),
    ok.

start_client(Args) ->
        case Args of
        [Host, PortString, Username] ->
            start(Username, Host, PortString);
        _ ->
            io:format("PollenClientModule: Expecting parameter <host> <port> <username>~n"),
            halt(1)
    end.

start(Username, Host, PortString) ->
    {Port, _} = string:to_integer(PortString),

    io:format("~nPollenClientModule: Connecting to ~s:~p...~n", [Host, Port]),
    
    case connect(Host, Port) of
        {ok, Sock} ->
            io:format("PollenClientModule: Connection to server established!~n"),
            
            %% Spawn new process for tcp
            ClientPid = spawn(fun() -> 
                register(client, self()),
                login(Username),
                loop(Sock)
            end),

            gen_tcp:controlling_process(Sock, ClientPid),

            %% Spawn new process for shell input
            spawn(fun() ->
                erlang:group_leader(whereis(user), self()),
                input_loop()
            end);

        {error, Reason} ->
            io:format("PollenClientModule: Failed to connect, ~p~n", [Reason]),
            halt(1)
    end.

connect(Host, Port) ->
    gen_tcp:connect(Host, Port, [{active, once}, {packet, 0}, {send_timeout, 5000}]).

loop(Sock) ->
    inet:setopts(Sock, [{active, once}]),

    receive
        {send, Request} ->
            gen_tcp:send(Sock, serialize(Request)),
            loop(Sock);

        {tcp, Sock, Data} ->
            dispatch(unserialize(Data)),
            loop(Sock);

        {tcp_closed, Sock} ->
            io:format("PollenClientModule: Connection closed by server.~n"),
            gen_tcp:close(Sock),
            halt(0);

        {error, timeout} ->
            io:format("PollenClientModule: Timeout.~n"),
            gen_tcp:close(Sock),
            halt(0)
    end.

%% ============================================

%% Dispatch actions
dispatch([{action, Action}, {payload, Payload}]) ->
    case Action of
        new_message -> 
            [{message, Message}] = Payload,
            io:format("~s~n", [Message]);

        new_login_message -> 
            print_command_list(),
            [{message, Message}] = Payload,
            io:format("~s~n", [Message]);

        _ -> 
            io:format("Unknown request ~p from server~n", [Action])
    end.

%% ============================================

login(Username) ->
    io:format("PollenClientModule: Attempting login as ~p...~n", [Username]),
    Action = login,
    Request = [{action, Action}, {payload, [{username, Username}]}],
    client ! {send, Request},
    ok.

ch_list() ->
    Action = ch_list,
    Request = [{action, Action}, {payload, []}],
    client ! {send, Request},
    ok.

new_channel(ChannelName) ->
    Action = ch_new,
    Request = [{action, Action}, {payload, [{channel_name, ChannelName}]}],
    client ! {send, Request},
    ok.

new_channel_private(ChannelName) ->
    Action = ch_new_priv,
    Request = [{action, Action}, {payload, [{channel_name, ChannelName}]}],
    client ! {send, Request},
    ok.

new_channel_private_with_invite(RecipientUsername, Message) ->
    Action = ch_new_priv_invite,
    Request = [{action, Action}, {payload, [{recipient, RecipientUsername}, {message, Message}]}],
    client ! {send, Request},
    ok.

invite_channel(RecipientUsername) ->
    Action = ch_invite,
    Request = [{action, Action}, {payload, [{recipient, RecipientUsername}]}],
    client ! {send, Request},
    ok.

close_channel() ->
    Action = ch_close,
    Request = [{action, Action}, {payload, []}],
    client ! {send, Request},
    ok.

join_channel(ChannelName) ->
    Action = ch_join,
    Request = [{action, Action}, {payload, [{channel_name, ChannelName}]}],
    client ! {send, Request},
    ok.

leave_channel() ->
    Action = ch_leave,
    Request = [{action, Action}, {payload, []}],
    client ! {send, Request},
    ok.

send_msg(Message) ->
    Action = send_msg,
    Request = [{action, Action}, {payload, [{message, Message}]}],
    client ! {send, Request},
    ok.

%% Basic ping request
ping() ->
    Action = ping,
    Request = [{action, Action}, {payload, []}],
    client ! {send, Request},
    ok.

unserialize(Data) -> binary_to_term(list_to_binary(Data)).
serialize(Request) -> term_to_binary(Request).

%% ============= Client Side =============

print_command_list() ->
    io:format("~nList of available commands:~n~n"),
    io:format("~-15s ~s~n", ["/help",       "-- Print again the list of commands"]),
    io:format("~-15s ~s~n", ["/list",       "-- List of all channels"]),
    io:format("~-15s ~s~n", ["/close",      "-- Close the channel that you're in, if you own it."]),
    io:format("~-15s ~s~n", ["/channel",    "-- Create a new channel"]),
    io:format("~-15s ~s~n", ["/invite",     "-- Add users to your private channel, if you own it"]),
    io:format("~-15s ~s~n", ["/pchannel",   "-- Create a new private channel"]),
    io:format("~-15s ~s~n", ["/pchannel",   "-- Leave current channel and return to (global)"]),
    io:format("~-15s ~s~n", ["/quit",       "-- Exit chat"]).

input_loop() ->
    Choice = string:trim(io:get_line(standard_io, "> ")),

    case Choice of
        "/list" ->
            ch_list(),
            input_loop();

        "/leave" ->
            leave_channel(),
            input_loop();

        %% TO BE DONE
        "/pmessage" ->
            RecipientUsername = string:trim(io:get_line(standard_io, "Insert the user you intend to message: ")),
            Message = string:trim(io:get_line(standard_io, "Insert the message: ")),
            new_channel_private_with_invite(RecipientUsername, Message),
            input_loop();

        %% TO BE DONE
        "/invite" ->
            RecipientUsername = string:trim(io:get_line(standard_io, "Insert user to invite: ")),
            invite_channel(RecipientUsername),
            input_loop();

        "/join" ->
            ChannelName = string:trim(io:get_line(standard_io, "Insert channel name: ")),
            join_channel(ChannelName),
            input_loop();

        "/close" ->
            close_channel(),
            input_loop();

        "/channel" ->
            ChannelName = string:trim(io:get_line(standard_io, "Insert new channel name: ")),
            new_channel(ChannelName),
            input_loop();

        "/pchannel" ->
            ChannelName = string:trim(io:get_line(standard_io, "Insert new private channel name: ")),
            new_channel_private(ChannelName),
            input_loop();

        "/quit" ->
            halt(0);
        
        "/help" ->
            print_command_list(),
            input_loop();
        
        _ ->
            send_msg(Choice),
            input_loop()
    end.
