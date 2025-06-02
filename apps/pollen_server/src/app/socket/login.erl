%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(login).

-export([handle/1, callback/3]).

-include("include/env.hrl").

%% Handle a new login request
handle([{username, Username}]) ->
    %% Store its sessions
    ?ENV_SERVER_LOGS andalso io:format("PollenLoginModule: Processing new login request. ~n"),

    %% Spawn a new client
    client:spawn_client(self(), Username).

callback(ClientPid, _ClientList, Username) ->
    {ok, Vsn} = application:get_key(pollen_server, vsn),

    %% Convert it to a user list for printing
    Response = [{action, new_login_message}, {payload, [{message, io_lib:format("~nPollen ~s~nWelcome ~s, you can begin chatting in this server.", [Vsn, Username])}]}],

    %% Send back the list to the user
    ClientPid ! {send, Response}.