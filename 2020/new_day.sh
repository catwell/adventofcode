#!/bin/bash

last_day_dir () { find . -type d -name 'day[012]*' | sort | tail -n1 ; }

new_day_dir () {
    local n="$(last_day_dir | tail -c3 | sed 's/^0*//')"
    printf "day%02d" "$(($n+1))"
}

dname="$(new_day_dir)"
cp -r template_day "$dname"
echo "Created $dname"
