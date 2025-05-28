-module(router).

-export([dispatch/2]).

%% Dispatch the request to the correct provider
dispatch(Request, Socket) ->
    [{endpoint, Endpoint}, {payload, Payload}] = Request,

    env:verbose() andalso io:format("PollenRouterModule: Endpoint is ~p~n", [Endpoint]),
    env:verbose() andalso io:format("PollenRouterModule: Payload is ~p~n", [Payload]),

    %% Router table
    case Endpoint of
        "ping"          -> ping:handle(Request, Socket);
        "user/login"    -> login:handle(Request, Socket);
        "room/new"      -> room:handle_new(Request, Socket);
        _               -> server:graceful_dconn(Socket, "POLEXCEPTION: Bad client request!")
    end.