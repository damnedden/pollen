%%%-------------------------------------------------------------------
%% @doc pollen server boot procedure.
%% @end
%%%-------------------------------------------------------------------

-module(tcp).

% Configuration
-define(MAX_PCK_SIZE, 2000). %% 0.2mB
-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([server/1, handle/1, graceful_dconn/2]).

%% Initial link called from supervisor
start_link(Port) ->
    io:format("Booting PollenTCPModule..."),

    case gen_tcp:listen(Port, [{active, false}, {packet, 0}, {reuseaddr, true}]) of
        {ok, ListenSock} ->
            io:format("~20s~n~n", ["OK"]),

            Pid = spawn_link(?MODULE, server, [ListenSock]),

            env:verbose() andalso io:format("PollenTCPModule: Server started, listening on port ~w.~n", [Port]),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%% Start multiple workers for each TCP
server(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSock} ->
            env:verbose() andalso io:format("PollenTCPModule: Accepting new connection.~n"),

            %% Spawn a new Pid to handle server side connection
            inet:setopts(ClientSock, [{active, false}, {packet, 0}]),
            Pid = spawn(?MODULE, handle, [ClientSock]),
            gen_tcp:controlling_process(ListenSock, Pid),

            %% Continue accepting new connections
            server(ListenSock);
        {error, Reason} ->
            env:verbose() andalso io:format("PollenTCPModule: Accept error: ~p.~n", [Reason]),
            ok
    end.

%% Gracefully disconnect a client with a custom message
graceful_dconn(Sock, Message) ->
    case gen_tcp:send(Sock, Message) of
        ok                  -> ok;
        {error, _Reason}    -> io:format("Failed to send disconnection message.~n")
    end,

    gen_tcp:close(Sock).    

%% Each client loops in its own process
handle(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Bin = term_to_binary(Data),
            Size = byte_size(Bin),

            %% Check the size of the packet
            if Size < ?MAX_PCK_SIZE ->
                Request = binary_to_term(list_to_binary(Data)),

                %% Dispatch the request and loop for new data
                router:dispatch(Request, Socket),

                handle(Socket);
            true ->
                env:verbose() andalso io:format("PollenTCPModule: Packet too large error (~wB).~n", [Size]),
                gen_tcp:close(Socket)
            end;
        {error, closed} ->
            env:verbose() andalso io:format("PollenTCPModule: Client disconnected.~n"),
            exit(self()),
            
            ok
    end.