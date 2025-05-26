%%%-------------------------------------------------------------------
%% @doc pollen server boot procedure.
%% @end
%%%-------------------------------------------------------------------

-module(server).

% Configuration
-define(MAX_PCK_SIZE, 2000). %% 0.2mB
-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([server/1, handle/1]).

%% Initial link called from supervisor
start_link(Port) ->
    case gen_tcp:listen(Port, [{active, false}, {packet, 0}, {reuseaddr, true}]) of
        {ok, ListenSock} ->
            Pid = spawn_link(?MODULE, server, [ListenSock]),

            %% Create a new table for storing sessions
            ets:new(pollen_sessions, [ordered_set, named_table]),

            env:verbose() andalso io:format("PollenServerModule: Server started, listening on port ~w.~n", [Port]),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

%% Start multiple workers for each TCP
server(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSock} ->
            env:verbose() andalso io:format("PollenServerModule: Accepting new connection.~n"),

            %% Spawn a new Pid to handle server side connection
            Pid = spawn(?MODULE, handle, [ClientSock]),
            gen_tcp:controlling_process(ListenSock, Pid),

            %% Continue accepting new connections
            server(ListenSock);
        {error, Reason} ->
            env:verbose() andalso io:format("PollenServerModule: Accept error: ~p.~n", [Reason]),
            ok
    end.

%% Each client loops in its own process
handle(Socket) ->
    inet:setopts(Socket, [{active, false}, {packet, 0}]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            Bin = term_to_binary(Data),
            Size = byte_size(Bin),

            %% Check the size of the packet
            if Size < ?MAX_PCK_SIZE ->
                Request = binary_to_term(list_to_binary(Data)),
                
                %% Dispatch the request
                router:dispatch(Request, Socket),

                %% Loop socket to listen
                handle(Socket);
            true ->
                env:verbose() andalso io:format("PollenServerModule: Packet too large error (~wB).~n", [Size]),
                gen_tcp:close(Socket)
            end;
        {error, Reason} ->
            env:verbose() andalso io:format("PollenServerModule: Client disconnected.~n"),
            {ok, Reason}
    end.