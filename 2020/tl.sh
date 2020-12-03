#!/bin/bash

die () { >&2 echo "$@"; exit 1 ; }

[ -z "$TL_BIN_PATH" ] && die "You must set $TL_BIN_PATH."

path="$(dirname "$0")"

"$path"/.lua/bin/lua "$TL_BIN_PATH" \
    --include-dir "$path" --skip-compat53 "$@"
