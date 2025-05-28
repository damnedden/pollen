-module(login).

-export([handle/2]).


%% Handle a new login request
handle(Request, Socket) ->
    [{endpoint, _Endpoint}, {payload, Payload}] = Request,
    [{username, Username}] = Payload,

    %% Store its sessions
    env:verbose() andalso io:format("PollenLoginModule: Processing new login request. ~n"),

    store(Username),

    %% Send the message to the client
    {ok, Vsn} = application:get_key(pollen_server, vsn),
    gen_tcp:send(Socket, io_lib:format("[ Pollen ~s ]~nWelcome ~s, you can begin chatting in this server.~n~nList of commands:~n- p:ping()~n- p:join()~n- p:room()", [Vsn, Username])),
    
    ok.

%% Store the new 
store(Username) -> user_register:new(Username).