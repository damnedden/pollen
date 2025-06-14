%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(ch_join).

-export([handle/1]).

handle([{channel_name, ChannelName}]) ->
    pollen_client_manager ! {switch_channel, self(), ChannelName},
    ok.