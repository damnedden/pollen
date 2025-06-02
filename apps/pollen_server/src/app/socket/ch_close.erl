%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(ch_close).

-export([handle/0]).

handle() ->
    pollen_channel_manager ! {close_channel, self()},
    ok.