#!/bin/sh

rebar3 release

exec _build/default/rel/pollen/bin/pollen foreground
