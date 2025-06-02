-module(ping).

-export([handle/0]).

handle() ->
    pollen_channel_manager ! {unicast, self(), "pong"},
    
    ok.