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

    PollenServer = #{
        id => server,
        start => {server, start_link, [4001]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [server]
    },

    ChildSpecs = [PollenServer],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
