-module(ch_new).

-export([handle/1, handle2/1]).

handle([{channel_name, ChannelName}]) ->
    channel:spawn_channel(pollen_channel_manager, self(), ChannelName, []),
    ok.

handle2([{channel_name, ChannelName}]) ->
    channel:spawn_channel(pollen_channel_manager, self(), ChannelName, [self()]),
    ok.