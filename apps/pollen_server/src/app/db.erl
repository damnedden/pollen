-module(db).

%% TODO 
%%
%% - keep-alive to check status of db connection

-behaviour(gen_server).

-export([start_link/4, init/1, handle_call/3, handle_cast/2]).

start_link(Secret, Access, Host, Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {Secret, Access, Host, Port}, []).

init({Secret, Access, Host, Port}) ->
    io:format("~nBooting PollenDatabaseModule..."),

    Client = aws_client:make_local_client(Access, Secret, Port, Host),

    % Input to list_tables can be an empty map if no filters/pagination needed
    Input = #{},

    % Call list_tables/2
    case aws_dynamodb:list_tables(Client, Input) of
        {ok, Output, _Meta} ->
            io:format("~15s~n", ["OK"]),
            
            {ok, self()};
        {error, Reason} when Reason == nxdomain -> {error, "Unable to connect to database, please check the endpoint."};
        {error, Reason} -> {error, Reason}
    end.


handle_call(Request, From, State) ->
    ok.

handle_cast(Request, State) ->
    ok.