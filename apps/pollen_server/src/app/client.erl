%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(client).
-include("include/env.hrl").

%% Public
- export([
    client_manager/1, 
    spawn_client/2
]).

-type client() :: #client{}.
-type client_list() :: [client()].

%% Channel and keep tracks of list of Clients [{Pid, Client}...]
-spec client_manager(client_list()) -> no_return().
client_manager(ClientList) ->
    receive
        %% Handle the switch of a channel
        {switch_channel, ClientPid, ChannelName} ->
            case get_client_by_pid(ClientList, ClientPid) of
                {ok, Client} -> pollen_channel_manager ! {join_channel, ChannelName, Client};
                ok -> ok
            end,
            
            client_manager(ClientList);

        %% Handle a new conversation
        {new_private_conversation, SenderPid, RecipientUsername, Message} ->
            case get_client_by_pid(ClientList, SenderPid) of
                {ok, SenderClient} -> 
                    case get_client_by_name(ClientList, RecipientUsername) of
                        {ok, RecipientClient} when RecipientClient#client.pid =:= SenderClient#client.pid -> channel:unicast(SenderPid, "You can't private message yourself.");
                        {ok, RecipientClient} -> channel:spawn_private_conversation(pollen_channel_manager, SenderClient, RecipientClient, Message);
                        ok -> channel:unicast(SenderPid, io_lib:format("User `~s` is currently not online.", [RecipientUsername]))
                    end;
                ok -> ok
            end,

            client_manager(ClientList);

        %% Handle a new invite
        {new_invite, SenderPid, RecipientUsername} ->
            case get_client_by_pid(ClientList, SenderPid) of
                {ok, SenderClient} -> 
                    case get_client_by_name(ClientList, RecipientUsername) of
                        {ok, RecipientClient} when RecipientClient#client.pid =:= SenderClient#client.pid -> channel:unicast(SenderPid, "You can't invite yourself.");
                        {ok, RecipientClient} -> pollen_channel_manager ! {invite, SenderClient, RecipientClient};
                        ok -> channel:unicast(SenderPid, io_lib:format("User `~s` is currently not online.", [RecipientUsername]))
                    end;
                ok -> ok
            end,

            client_manager(ClientList);

        %% Add a new client to the list
        {add, Client} ->
            case client_list_add(Client, ClientList) of
                {ok, ClientList2} ->
                    ClientPid = Client#client.pid,
                    erlang:monitor(process, ClientPid),
                    pollen_channel_manager ! {join_channel, ?ENV_GLOBAL_CH_NAME, Client},
                    login:callback(ClientPid, ClientList2, Client#client.name),
                    ?ENV_SERVER_LOGS andalso io:format("PollenClientModule: Spawning new client ~p with PID ~p~n", [Client#client.name, ClientPid]),
                    client_manager(ClientList2);

                error ->
                    client_manager(ClientList)
            end;
            
        %% Client was terminated abruptely
        {'DOWN', _MonitorRef, process, ClientPid, _Reason} ->
            case get_client_by_pid(ClientList, ClientPid) of
                {ok, Client} -> pollen_channel_manager ! {leave_channel, Client};
                [] -> ok
            end,

            ClientList2 = client_list_remove(ClientPid, ClientList),
            client_manager(ClientList2)
    end.

%% Add Clients to the manager
-spec client_list_add(client(), client_list()) -> {ok, client_list()} | error.
client_list_add(Client, ClientList) ->
    case get_client_by_name(ClientList, Client#client.name) of
        {ok, _Found} ->
            Client#client.pid ! {tcp_exception, client_already_exist},
            error;
        ok ->
            {ok, [Client | ClientList]}
    end.

%% Remove Clients from the manager
-spec client_list_remove(pid(), client_list()) -> client_list().
client_list_remove(ClientPid, ClientList) -> lists:filter(fun(U) -> U#client.pid =/= ClientPid end, ClientList).

-spec get_client_by_name(client_list(), string()) -> {ok, client()} | ok.
get_client_by_name(ClientList, ClientName) -> 
    case lists:filter(fun(U) -> U#client.name == ClientName end, ClientList) of
        [First | _] -> {ok, First};
        [] -> ok
    end.

-spec get_client_by_pid(client_list(), pid()) -> {ok, client()} | ok.
get_client_by_pid(ClientList, ClientPid) -> 
    case lists:filter(fun(U) -> U#client.pid == ClientPid end, ClientList) of
        [First | _] -> {ok, First};
        [] -> ok
    end.

-spec random_color() -> string().
random_color() ->
    ColorMap = #{
        1 => "\e[31m", 
        2 => "\e[35m", 
        3 => "\e[37m", 
        4 => "\e[36m",
        5 => "\e[34m"
    },

    maps:get(rand:uniform(5), ColorMap).

%%%% -------------------------------------------------------------------
%%% Client spawning
%%% -------------------------------------------------------------------

-spec spawn_client(pid(), string()) -> no_return().
spawn_client(ClientPid, Name) ->
    Client = #client{name=Name, pid=ClientPid, color=random_color()},
    pollen_client_manager ! {add, Client}.