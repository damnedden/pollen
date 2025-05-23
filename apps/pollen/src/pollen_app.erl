%%%-------------------------------------------------------------------
%% @doc pollen public API
%% @end
%%%-------------------------------------------------------------------

-module(pollen_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollen_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
