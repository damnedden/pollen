%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

-module(ch_invite).

-export([handle/1]).

handle([{recipient, RecipientName}]) ->
    pollen_client_manager ! {new_invite, self(), RecipientName},
    ok.