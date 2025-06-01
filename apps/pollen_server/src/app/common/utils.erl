- module(utils).
- export([serialize/1,clist_to_ulist/1]).

clist_to_ulist([]) -> 
    [];

clist_to_ulist([{_Pid, Username} | Tail]) -> 
    [Username | clist_to_ulist(Tail)].

serialize(Request) -> term_to_binary(Request).