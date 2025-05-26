#!/usr/bin/env escript
%%! -sname pollen_client

%% Client side of the application
-module(start).

-export([main/1]).

%% Main function called at the run
main(Args) ->
    case Args of
        [Host, PortString, Username] ->
            %% Convert the port to integer
            {Port, _} = string:to_integer(PortString),

            io:format("PollenClientModule: Attempting to connect to ~p:~p~n", [Host, Port]),
            case connect(Host, Port) of
                {ok, Sock} ->
                    io:format("PollenClientModule: Successfully connected to server!~n"),
                    login(Sock, Username),
                    loop(Sock);
                {error, Reason} ->
                    io:format("PollenClientModule: Failed to connect, ~p~n", [Reason]),
                    halt(1)
            end;
        _ ->
            io:format("PollenClientModule: Expecting parameter <host> <port> <username>~n"),
            halt(1)
    end.

connect(Host, Port) ->
    gen_tcp:connect(Host, Port, [{active, true}, {packet, 0}]).

login(Sock, Username) ->
    io:format("PollenClientModule: Attempting login.~n"),
    Endpoint = "login",
    Request = [{endpoint, Endpoint}, {payload, [{username, Username}]}],
    gen_tcp:send(Sock, serialize(Request)).

loop(Sock) ->
    receive
        %%{send, Request} ->
        %%    %% Prepare the request
        %%    [{endpoint, Endpoint}, {payload, Payload}] = Request,
        %%    io:format("PollenClientModule: ~p calling endpoint ~p ~n", [self(), Endpoint]),
%%
        %%    %% Send it to the server
        %%    gen_tcp:send(Sock, serialize(Request)),
%%
        %%    %% Finally loop again for new listener
        %%    loop(Sock);

        {tcp, Sock, Data} ->
            io:format("PollenClientModule: ~p~n", [Data]),
            loop(Sock);
        {tcp_closed, Sock} ->
            io:format("PollenClientModule: Connection closed by server.~n"),
            halt(0)
    end.

serialize(Request) ->
    term_to_binary(Request).
