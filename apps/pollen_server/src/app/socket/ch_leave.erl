-module(ch_leave).

-export([handle/0]).

handle() ->
    pollen_client_manager ! {switch_channel, self(), "(global)"},
    ok.