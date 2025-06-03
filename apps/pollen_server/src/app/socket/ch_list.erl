%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(ch_list).

-export([handle/0, callback/2]).

- spec handle() -> no_return().
handle() ->
    pollen_channel_manager ! {list_channels, self()},
    ok.

callback(ClientPid, ChannelList) ->
    ClientPid ! {send, [{action, new_message}, {payload, [{message, "Available channels to join:" ++ io_lib:format("~p", [ChannelList])}]}]},
    ok.
