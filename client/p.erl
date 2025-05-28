%% Client side of the application
-module(p).

-export([main/1]).
-export([ping/0]).

%% Main function called at the run
main(Args) ->
    start_client(Args),
    ok.

start_client(Args) ->
        case Args of
        [Host, PortString, Username] ->
            %% Convert the port to integer
            {Port, _} = string:to_integer(PortString),

            io:format("~nPollenClientModule: Connecting to ~s:~p...~n", [Host, Port]),
            
            case connect(Host, Port) of
                {ok, Sock} ->
                    io:format("PollenClientModule: Successfully connected to server!~n"),
                    
                    %% Create a new process to keep the shell free
                    ClientPid = spawn(fun() -> 
                        register(client, self()),
                        loop(Sock) 
                    end),
                    
                    gen_tcp:controlling_process(Sock, ClientPid),

                    login(Username);
                {error, Reason} ->
                    io:format("PollenClientModule: Failed to connect, ~p~n", [Reason]),
                    halt(1)
            end;
        _ ->
            io:format("PollenClientModule: Expecting parameter <host> <port> <username>~n"),
            halt(1)
    end.

connect(Host, Port) ->
    gen_tcp:connect(Host, Port, [{active, true}, {packet, 0}]).

loop(Sock) ->
    receive
        {send, Request} ->
            gen_tcp:send(Sock, utils:serialize(Request)),
            loop(Sock);
        {tcp, Sock, Data} ->
            io:format("~s~n~n> ", [Data]),
            loop(Sock);
        {tcp_closed, Sock} ->
            io:format("PollenClientModule: Connection closed by server.~n"),
            halt(0)
    end.

%% ============================================

%% Initial call made by the application to login into a pollen server
login(Username) ->
    io:format("PollenClientModule: Attempting login as ~p...~n", [Username]),
    Endpoint = "user/login",
    Request = [{endpoint, Endpoint}, {payload, [{username, Username}]}],
    client ! {send, Request}.

%% Basic ping request
ping() ->
    Endpoint = "ping",
    Request = [{endpoint, Endpoint}, {payload, []}],
    client ! {send, Request},
    ok.