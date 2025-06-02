%%% -*- erlang -*-
%%%
%%% This file is part of pollen released under the Apache 2 license.
%%% See the NOTICE for more information.
%%%
%%% Copyright (c) 2025-2026, Daniele Fiore <daniele.fiore.work1+person@gmail.com>
%%%

- module(utils).
- export([serialize/1,clist_to_ulist/1]).

clist_to_ulist([]) -> 
    [];

clist_to_ulist([{_Pid, Username} | Tail]) -> 
    [Username | clist_to_ulist(Tail)].

serialize(Request) -> term_to_binary(Request).