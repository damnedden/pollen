-module(ch_list).

-export([handle/0, callback/2]).

handle() ->
    pollen_channel_manager ! {list_channels, self()},
    ok.

callback(ClientPid, ChannelList) ->
    ClientPid ! {send, [{action, new_message}, {payload, [{message, "Available channels to join:" ++ io_lib:format("~p", [ChannelList])}]}]},
    ok.
