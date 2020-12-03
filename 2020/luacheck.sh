#!/bin/bash

die () { >&2 echo "$@"; exit 1 ; }

path="$(dirname "$0")"

check_one () {
    fn="$(mktemp "/tmp/XXXXXX-$(basename $1).lua")"
    "$path"/tl.sh gen --output "$fn" "$1" || die
    luacheck --config "$path"/.luacheckrc "$fn"
    rm "$fn"
}

if [ -z "$1" ]; then
    for f in day*/*.tl; do
        check_one "$f"
    done
elif [ -d "$1" ]; then
    for f in "$1"/*.tl; do
        check_one "$f"
    done
else
    check_one "$1"
fi
