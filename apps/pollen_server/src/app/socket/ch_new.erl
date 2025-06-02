-module(ch_new).

-export([handle/1, handle_priv/1, handle_priv_with_invite/1]).

%% Spawn a new channel
handle([{channel_name, ChannelName}]) ->
    channel:spawn_channel(pollen_channel_manager, self(), ChannelName, []),
    ok.

%% Spawn a new private channel
handle_priv([{channel_name, ChannelName}]) ->
    channel:spawn_channel(pollen_channel_manager, self(), ChannelName, [self()]),
    ok.

%% Message the client manager to init a converstaion between 2 users
handle_priv_with_invite([{recipient, RecipientUsername}, {message, Message}]) ->
    pollen_client_manager ! {new_private_conversation, self(), RecipientUsername, Message},
    ok.