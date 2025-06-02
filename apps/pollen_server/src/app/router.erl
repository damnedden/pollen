-module(router).

-export([dispatch/1]).

%% Dispatch the request
dispatch([{action, Action}, {payload, Payload}]) ->
    case Action of
        send_msg            -> send_msg:handle(Payload);
        ch_close            -> ch_close:handle();
        ch_new              -> ch_new:handle(Payload); 
        ch_new_priv_invite  -> ch_new:handle_priv_with_invite(Payload);
        ch_new_priv         -> ch_new:handle_priv(Payload);    
        ch_leave            -> ch_leave:handle();
        ch_join             -> ch_join:handle(Payload);
        ch_list             -> ch_list:handle();
        login               -> login:handle(Payload);
        ping                -> ping:handle();
        _                   -> self() ! {tcp_exception, bad_request}
    end.