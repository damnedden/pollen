- module(utils).
- export([serialize/1]).

serialize(Request) -> term_to_binary(Request).