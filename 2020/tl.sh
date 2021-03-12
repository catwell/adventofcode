#!/bin/bash

die () { >&2 echo "$@"; exit 1 ; }

[ -z "$TL_BIN_PATH" ] && die "You must set $TL_BIN_PATH."

path="$(dirname "$0")"
lua="$path/.lua/bin/lua"

lua_path="$("$lua" -e 'print(package.path)')"

LUA_PATH="$(dirname $TL_BIN_PATH)/?.lua;$lua_path" "$lua" "$TL_BIN_PATH" \
    --include-dir "$path" --skip-compat53 "$@"
