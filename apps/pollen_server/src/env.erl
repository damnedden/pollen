-module(env).

-export([verbose/0]).

%% Easier to debug, prints messaegs
verbose() ->
    case application:get_env(pollen, verbose) of
        {ok, 1} -> true;
        _       -> false
    end.