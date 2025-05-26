-module(router).

-export([dispatch/2]).

%% Dispatch the request to the correct provider
dispatch(Request, Socket) ->
    [{endpoint, Endpoint}, {payload, Payload}] = Request,

    env:verbose() andalso io:format("PollenRouterModule: Endpoint is ~p~n", [Endpoint]),
    env:verbose() andalso io:format("PollenRouterModule: Payload is ~p~n", [Payload]),

    case Endpoint of
        "login" -> login:handle(Request, Socket);
        _       -> io:format("PollenRouterModule: Endpoint not found~n")
    end.