%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(ch_leave).

-export([handle/0]).

handle() ->
    pollen_client_manager ! {switch_channel, self(), "(global)"},
    ok.