-module(room).

-export([handle_new/2]).

handle_new(Request, Socket) ->
    [{endpoint, _Endpoint}, {payload, Payload}] = Request,
    [{username, Username}] = Payload,

    %% Store its sessions
    io:format("PollenLoginModule: This call was made from pid ~w~n", [self()]),

    %% Send the message to the client
    {ok, Vsn} = application:get_key(pollen_server, vsn),
    gen_tcp:send(Socket, io_lib:format("Pollen ~s ~n Welcome ~s, choose your action~n", [Vsn, Username])),
    
    ok.
