#!/bin/sh

_build/default/rel/pollen/bin/pollen daemon &

exec _build/default/rel/pollen/bin/pollen daemon_attach
