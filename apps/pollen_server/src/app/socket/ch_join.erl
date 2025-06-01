-module(ch_join).

-export([handle/1]).

handle([{channel_name, ChannelName}]) ->
    pollen_client_manager ! {switch_channel, self(), ChannelName},
    ok.