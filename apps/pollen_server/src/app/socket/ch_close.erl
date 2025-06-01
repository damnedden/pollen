-module(ch_close).

-export([handle/0]).

handle() ->
    pollen_channel_manager ! {close_channel, self()},
    ok.