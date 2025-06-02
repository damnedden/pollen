%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(send_msg).

-export([handle/1]).

handle([{message, Message}]) ->
    ClientPid = self(),
    pollen_channel_manager ! {broadcast, ClientPid, Message},
    ok.