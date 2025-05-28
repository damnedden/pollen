-module(ping).

-export([handle/2]).

handle(Request, Socket) ->
    [{endpoint, _Endpoint}, {payload, _Payload}] = Request,

    %% Store its sessions
    io:format("PollenLoginModule: Pong ~p~n", [self()]),

    %% Pong to user
    gen_tcp:send(Socket, "pong"),
    
    ok.