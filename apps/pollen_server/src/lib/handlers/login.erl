-module(login).

-export([handle/2]).

%% Handle a new login request
handle(Request, Socket) ->
    [{endpoint, Endpoint}, {payload, Payload}] = Request,
    [{username, Username}] = Payload,

    %% Store its sessions
    io:format("PollenLoginModule: Storing pid ~w and username ~p inside pollen_sessions  ~n", [self(), Username]),
    %% ets:insert(pollen_sessions, {self(), Username}),

    %% Send the message to the client
    {ok, Version} = application:get_key(pollen_server, vsn),
    gen_tcp:send(Socket, io_lib:format("Welcome ~p to Pollen ~p", [Username, Version])),
    
    ok.
