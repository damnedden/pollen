%%%-------------------------------------------------------------------
%% @doc pollen top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollen_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 0,
        period => 1
    },

    PollenDbModule = #{
        id => pollen_db,
        start => {db, start_link, [<<"ASIAY34FZKBOKMUTVV7A">>, <<"ASIAY34FZKBOKMUTVV7A">>, <<"pollen-dynamodb">>, <<"8000">>]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [db]
    },

    PollenTcpServer = #{
        id => pollen_tcp_server,
        start => {tcp, start_link, [4000]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [tcp]
    },

    ChildSpecs = [PollenDbModule, PollenTcpServer],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
