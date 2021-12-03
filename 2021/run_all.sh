#!/bin/bash

for day in day*; do
    echo $day
    pushd $day >/dev/null
    for part in part*.tl; do
        ../tl.sh run $part
    done
    popd >/dev/null
done
