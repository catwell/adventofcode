#!/bin/bash

day="$(ls | grep ^day | tail -n1)"

echo $day
pushd $day >/dev/null
for part in part*.tl; do
    ../tl.sh run $part
done
popd >/dev/null
