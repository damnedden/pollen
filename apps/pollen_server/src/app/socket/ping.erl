-module(ping).

-export([handle/0]).

handle() ->
    io:format("PollenLoginModule: Pong ~p~n", [self()]),

    self() ! {send, [{action, new_message}, {payload, [{message, "pong"}]}]},
    
    ok.