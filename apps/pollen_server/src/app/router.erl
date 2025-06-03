%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(router).

-export([dispatch/1]).

%% Dispatch the request
-spec dispatch({{action, atom()}, {payload, any()}}) -> no_return().
dispatch([{action, Action}, {payload, Payload}]) ->
    case Action of
        send_msg                -> send_msg:handle(Payload);
        ch_close                -> ch_close:handle();
        ch_invite               -> ch_invite:handle(Payload); 
        ch_new                  -> ch_new:handle(Payload); 
        ch_new_priv_invite      -> ch_new:handle_priv_with_invite(Payload);
        ch_new_priv             -> ch_new:handle_priv(Payload);    
        ch_leave                -> ch_leave:handle();
        ch_join                 -> ch_join:handle(Payload);
        ch_list                 -> ch_list:handle();
        login                   -> login:handle(Payload);
        ping                    -> ping:handle();
        _                       -> self() ! {tcp_exception, bad_request}
    end.