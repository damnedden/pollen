-module(channel).

-include("include/env.hrl").

%% Export
- export([channel_manager/1, spawn_channel/3, spawn_channel/4, spawn_global_channel/1]).

%% Instantiate the channel manager
channel_manager(State = {ChannelList, ClientsMap}) ->
    receive
        %% Broadcast to the channel the pid is currently connected
        {broadcast, ClientPid, Message} ->
            case get_channel_by_client_pid(ClientPid, ClientsMap, ChannelList) of
                {ok, Channel} ->
                    Client = get_client_in_clientmap(ClientPid, ClientsMap),
                    FormattedMessage = io_lib:format("~s~s\e[0m@~s ~s", [Client#client.color, Client#client.name, Channel#channel.name, Message]),
                    Channel#channel.pid ! {broadcast, FormattedMessage};
                error -> send_disconnected_message(ClientPid)
            end,
            
            channel_manager(State);

        %% User join a new channel
        {join_channel, ChannelName, Client} ->
            ClientPid = Client#client.pid,

            case get_channel_by_name(ChannelList, ChannelName) of
                {ok, Channel} ->
                    case is_client_whitelisted(Channel#channel.whitelist, ClientPid) of
                        true ->
                            case get_channel_by_client_pid(ClientPid, ClientsMap, ChannelList) of
                                {ok, #channel{pid=OldChannelPid}} when OldChannelPid =/= Channel#channel.pid -> 
                                    OldChannelPid ! {leave, Client};
                                {ok, #channel{pid=OldChannelPid, name=OldChannelName}} when OldChannelPid =:= Channel#channel.pid -> 
                                    ClientPid ! {send, [{action, new_message}, {payload, [{message, io_lib:format("You are already in `~s`!", [OldChannelName])}]}]},
                                    channel_manager(State);
                                error -> send_disconnected_message(ClientPid)
                            end,

                            NewClientsMap = maps:put(ClientPid, Client, ClientsMap),
                            Channel#channel.pid ! {join, Client},
                            channel_manager({ChannelList, NewClientsMap});
                        false ->
                            ClientPid ! {send, [{action, new_message}, {payload, [{message, io_lib:format("Channel `~s` is private, ask the owner for an invite with /invite.", [ChannelName])}]}]},
                            channel_manager(State)
                    end;
                error ->
                    ClientPid ! {send, [{action, new_message}, {payload, [{message, io_lib:format("Channel `~s` not found.", [ChannelName])}]}]},
                    channel_manager(State)
            end;

        %% Request to close channel coming from client
        {close_channel, ClientPid} ->
            case get_channel_by_client_pid(ClientPid, ClientsMap, ChannelList) of
                {ok, #channel{name=ChannelName, owner=Owner, pid=ChannelPid}} when Owner =:= ClientPid -> ChannelPid !{terminate, user_requested_closing};
                {ok, #channel{name=ChannelName}} -> ClientPid ! {send, [{action, new_message}, {payload, [{message, io_lib:format("Channel `~s` cant be closed.", [ChannelName])}]}]};
                error -> send_disconnected_message(ClientPid)
            end,
            channel_manager(State);

        %% Client disconnect, broadcast it to every client in channel
        {leave_channel, Client = #client{pid=ClientPid}} ->
            case get_channel_by_client_pid(ClientPid, ClientsMap, ChannelList) of
                {ok, Channel} ->
                    Channel#channel.pid ! {leave, Client};
                error ->
                    ok
            end,

            NewClientsMap = maps:remove(ClientPid, ClientsMap),
            channel_manager({ChannelList, NewClientsMap});

        %% Register a new channel
        {register, Channel} ->
            ChannelPid = spawn(fun() -> channel(Channel) end),
            NewChannel = Channel#channel{pid = ChannelPid},

            %% If the channel doesn't exist already
            case ch_list_add(NewChannel, ChannelList) of
                {ok, ChannelList2} ->
                    case get_channel_by_name(ChannelList2, ?ENV_GLOBAL_CH_NAME) of
                        {ok, GlobalChannel} -> 
                            case Channel#channel.whitelist =:= [] of
                                true -> 
                                    FormattedMessage = io_lib:format("\e[32mNew channel `~s` was created.\e[0m", [Channel#channel.name]),
                                    GlobalChannel#channel.pid ! {broadcast, FormattedMessage};
                                false -> ok
                            end;
        
                        error -> ok
                    end,  

                    erlang:monitor(process, ChannelPid),
                    channel_manager({ChannelList2, ClientsMap});

                ok ->
                    channel_manager(State)
            end;

        %% List all channels a client can see
        {list_channels, ClientPid} ->
            VisibleChannelList = get_list_visible_channels_for_pid(ClientPid, ChannelList),
            FormattedChannelList = lists:map(fun(C) -> C#channel.name end, VisibleChannelList),
            ch_list:callback(ClientPid, FormattedChannelList),
            channel_manager(State);
        
        %% List of all channels server side
        {list_channels} ->
            io:format("PollenChannelModule: ~p~n", [ChannelList]),
            channel_manager(State);

        %% List all users, siblings of client pid in a channel
        {get_users_connected, ClientPid} ->
            case get_channel_by_client_pid(ClientPid, ClientsMap, ChannelList) of
                {ok, Channel} ->
                    Channel#channel.pid ! {list_members, ClientPid};
                error -> send_disconnected_message(ClientPid)
            end,
 
            channel_manager(State);

        %% Channel terminated
        {'DOWN', _MonitorRef, process, ChannelPid, Reason} ->
            io:format("PollenChannelModule: Channel ~p terminated abruptely ~p ~n", [ChannelPid, Reason]),
            NewChannelList = ch_list_remove(ChannelPid, ChannelList),
            
            %% If its the global channel down attempt a restart
            case get_channel_by_name(NewChannelList, ?ENV_GLOBAL_CH_NAME) of
                {ok, _Channel} -> ok;
                error ->
                    io:format("PollenChannelModule: Attempting to restart global channel~n"),
                    spawn_global_channel(self())
            end,   

            channel_manager({NewChannelList, ClientsMap});

        terminate ->
            ok
    end.

%% Loop for requests to process
channel_loop(Channel) ->
    receive
        %% Broadcast to all members
        {broadcast, Message} -> 
            broadcast(Channel, Message),
            channel_loop(Channel);

        %% List all members of the channel
        {list_members, ClientPid} ->
            Members = lists:filter(fun(C) -> C =/= Channel#channel.pid end, pg:get_members(Channel#channel.id)),
            user_list:callback(ClientPid, Members),
            channel_loop(Channel);

        %% List whitelist
        {get_whitelist, ClientPid} ->
            channel_loop(Channel);

        %% Client joined
        {join, Client = #client{name=Name, pid=ClientPid, color=ClientColor}} ->
            pg:join(Channel#channel.id, ClientPid),
            ClientPid ! {send, [{action, new_message}, {payload, [{message, io_lib:format("\e[32mConnected to `~s` channel!\e[0m", [Channel#channel.name])}]}]},
            broadcast(Channel, io_lib:format("~s~s joined the channel.\e[0m", [ClientColor, Name])),
            channel_loop(Channel);

        %% Client left
        {leave, Client = #client{name=Name, pid=ClientPid, color=ClientColor}} ->
            pg:leave(Channel#channel.id, ClientPid),
            broadcast(Channel, io_lib:format("~s~s left the channel.\e[0m", [ClientColor, Name])),
            channel_loop(Channel);

        terminate ->
            ok;

        {terminate, Reason} ->
            broadcast(Channel, io_lib:format("\e[33mThe owner of this channel terminated the session, please join a channel with /join.\e[0m", [])),
            io:format("PollenChannelModule: ~s, terminating channel.~n", [Reason]),
            ok;
        
        _Other ->
            channel_loop(Channel)
    end.

%% Registry for channels
ch_list_add(NewChannel, ChannelList) ->
    case get_channel_by_name(ChannelList, NewChannel#channel.name) of
        {ok, Channel} ->
            NewChannel#channel.pid ! {terminate, channel_already_exist},
            ok;
        error ->
            {ok, [NewChannel | ChannelList]}
    end.

ch_list_remove(ChannelPid, ChannelList) -> lists:filter(fun(C) -> C#channel.pid =/= ChannelPid end, ChannelList).

%% Channel function for each
channel(Channel) ->
    io:format("PollenChannelModule: Spawning new channel ~p with PID ~p~n", [Channel#channel.name, self()]),
    pg:join(Channel#channel.id, self()),
    NewChannel = Channel#channel{pid = self()},
    channel_loop(NewChannel).

%% Broadcast a message to all processes of a group 
broadcast(Channel, Message) ->
    Children = pg:get_members(Channel#channel.id),
    lists:map(fun(C) -> C ! {send, [{action, new_message}, {payload, [{message, Message}]}]} end, Children).

%%%% -------------------------------------------------------------------
%%% Helper
%%% -------------------------------------------------------------------

get_channel_by_name(ChannelList, ChannelName) -> 
    case lists:filter(fun(C) -> C#channel.name == ChannelName end, ChannelList) of
        [First | _] -> {ok, First};
        [] -> error
    end.

get_channel_by_client_pid(ClientPid, ClientsMap, ChannelList) ->
    case maps:is_key(ClientPid, ClientsMap) of
        true ->
            ChannelFound = lists:filter(fun(C) -> Members = pg:get_members(C#channel.id), lists:member(ClientPid, Members)end,ChannelList),
            case ChannelFound of
                [Channel | _] -> {ok, Channel};
                [] -> error
            end;
        false -> error
    end.

get_client_in_clientmap(ClientPid, ClientsMap) ->
    case maps:get(ClientPid, ClientsMap, undefined) of
        undefined -> error;
        Client -> Client
    end.

is_client_whitelisted([], ClientPid) -> true;
is_client_whitelisted(Whitelist, ClientPid) ->  lists:member(ClientPid, Whitelist).
get_list_visible_channels_for_pid(ClientPid, ChannelList) -> lists:filter(fun(Channel) -> is_client_whitelisted(Channel#channel.whitelist, ClientPid) end, ChannelList).
send_disconnected_message(ClientPid) -> ClientPid ! {send, [{action, new_message}, {payload, [{message, "You are not connected to any channel."}]}]}.

%%%% -------------------------------------------------------------------
%%% Channel spawning
%%% -------------------------------------------------------------------

spawn_channel(ChannelManagerPid, ClientPid, Name, Whitelist) ->
    Global = #channel{id=?ENV_CH_ID_PREFIX ++ Name, name=Name, owner=ClientPid, whitelist=Whitelist},
    ChannelManagerPid ! {register, Global}.

spawn_channel(ChannelManagerPid, ClientPid, Name) ->
    spawn_channel(ChannelManagerPid, ClientPid, Name, []).

spawn_global_channel(ChannelManagerPid) ->
    spawn_channel(ChannelManagerPid, ChannelManagerPid, ?ENV_GLOBAL_CH_NAME).