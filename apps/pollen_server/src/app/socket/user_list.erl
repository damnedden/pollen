-module(user_list).

-export([handle/0,callback/2]).

handle() ->
    ClientPid = self(),
    pollen_channel_manager ! {get_users_connected, ClientPid},
    ok.

callback(ClientPid, Siblings) ->
    ClientPid ! {send, [{action, new_message}, {payload, [{message, io_lib:format("~p.", [Siblings])}]}]}.