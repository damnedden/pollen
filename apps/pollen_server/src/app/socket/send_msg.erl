-module(send_msg).

-export([handle/1]).

handle([{message, Message}]) ->
    ClientPid = self(),
    pollen_channel_manager ! {broadcast, ClientPid, Message},
    ok.