%%%-------------------------------------------------------------------
%% @doc pollen public API
%% @end
%%%-------------------------------------------------------------------

-module(pollen_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(hackney),
    application:ensure_all_started(pg),
    pollen_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
