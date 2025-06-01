%%%-------------------------------------------------------------------
%% @doc pollen server boot procedure.
%% @end
%%%-------------------------------------------------------------------

-module(tcp).

-include("include/env.hrl").

% Configuration
-define(MAX_PCK_SIZE, 2000). %% 0.2mB
-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([acceptor/1, handle_client/1]).

%% Initial link called from supervisor
start_link(Port) ->
    io:format("Booting PollenTCPModule..."),

    case gen_tcp:listen(Port, [{active, once}, {packet, 0}, {reuseaddr, true}]) of
        {ok, ListenSock} ->
            io:format("~20s~n~n", ["OK"]),
            
            AcceptorPid = spawn_link(?MODULE, acceptor, [ListenSock]),
            ClientManagerPid = spawn_link(client, client_manager, [[]]),
            ChannelManagerPid = spawn_link(channel, channel_manager, [{[], #{}}]),
            
            pg:start_link(),
            channel:spawn_global_channel(ChannelManagerPid),
            channel:spawn_channel(ChannelManagerPid, AcceptorPid, "pub1"),
            channel:spawn_channel(ChannelManagerPid, AcceptorPid, "pub2", [AcceptorPid]),

            register(pollen_acceptor, AcceptorPid),
            register(pollen_client_manager, ClientManagerPid),
            register(pollen_channel_manager, ChannelManagerPid),

            ?ENV_SERVER_LOGS andalso io:format("PollenTCPModule: Server started, listening on port ~w.~n", [Port]),
            
            {ok, self()};
        {error, Reason} ->
            {error, Reason}
    end.

%% Start multiple workers for each TCP
acceptor(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, ClientSock} ->
            ?ENV_SERVER_LOGS andalso io:format("PollenTCPModule: Accepted new connection~n"),
            
            %% Spawn a new Pid to handle server side connection
            inet:setopts(ClientSock, [{active, once}, {packet, 0}]),
            Pid = spawn(?MODULE, handle_client, [ClientSock]),
            gen_tcp:controlling_process(ClientSock, Pid),

            %% Continue accepting new connections
            acceptor(ListenSock);
        {error, Reason} ->
            ?ENV_SERVER_LOGS andalso io:format("PollenTCPModule: Accept error: ~p.~n", [Reason]),
            ok
    end.

%% TCP for each client node
handle_client(Socket) -> loop(Socket).

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {send, Response} ->
            gen_tcp:send(Socket, utils:serialize(Response)),

            loop(Socket);
        {tcp, Socket, Data} ->
            Bin = term_to_binary(Data),
            Size = byte_size(Bin),
            Request = binary_to_term(list_to_binary(Data)),

            case Size < ?MAX_PCK_SIZE of
                true ->
                    router:dispatch(Request),
                    loop(Socket);
                false ->
                    ?ENV_SERVER_LOGS andalso io:format("PollenTCPModule: Terminating client, packet payload is too large (~p bytes)~n", [byte_size(Bin)]),
                    gen_tcp:close(Socket)
            end;

        {tcp_closed, Socket} ->
            ?ENV_SERVER_LOGS andalso io:format("PollenTCPModule: Client disconnected.~n"),
            pollen_client_manager ! {remove, self()};

        {tcp_error, Socket, Reason} ->
            ?ENV_SERVER_LOGS andalso io:format("PollenTCPModule: TCP error: ~p~n", [Reason]),
            pollen_client_manager ! {remove, self()};

        {tcp_exception, Reason} ->
            ?ENV_SERVER_LOGS andalso io:format("PollenTCPModule: ~p, terminating client...~n", [Reason]),
            pollen_client_manager ! {remove, self()};

        terminate ->
            ok
    end.