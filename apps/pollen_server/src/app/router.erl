-module(router).

-export([dispatch/1]).

%% Dispatch the request
dispatch([{action, Action}, {payload, Payload}]) ->
    case Action of
        send_msg        -> send_msg:handle(Payload);
        ch_new          -> ch_new:handle(Payload); 
        ch_close        -> ch_close:handle();      
        ch_new_priv     -> ch_new:handle2(Payload);    
        ch_leave        -> ch_leave:handle();
        ch_join         -> ch_join:handle(Payload);
        ch_list         -> ch_list:handle();
        user_list       -> user_list:handle();
        login           -> login:handle(Payload);
        ping            -> ping:handle();
        _               -> self() ! {tcp_exception, bad_request}
    end.