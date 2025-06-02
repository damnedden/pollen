%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(pollen_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(hackney),
    application:ensure_all_started(pg),
    pollen_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
