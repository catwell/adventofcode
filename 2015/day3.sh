td="/tmp/day3"

visit () { touch "x${1}y${2}" ; }

move_santa () {
    case $1 in
        "<") sx=$((sx-1)) ;;
        ">") sx=$((sx+1)) ;;
        "v") sy=$((sy-1)) ;;
        "^") sy=$((sy+1)) ;;
    esac
}

move_robot () {
    case $1 in
        "<") rx=$((rx-1)) ;;
        ">") rx=$((rx+1)) ;;
        "v") ry=$((ry-1)) ;;
        "^") ry=$((ry+1)) ;;
    esac
}

prepare () {
    rm -rf "$td"
    mkdir "$td"
}

part1 () {
    >/dev/null pushd "$td"
    sx=0; sy=0
    visit 0 0
    for i in $input; do
        move_santa $i
        visit $sx $sy
    done
    ls /tmp/day3 | wc -l
    >/dev/null popd
}

part2 () {
    >/dev/null pushd "$td"
    sx=0; sy=0; rx=0; ry=0
    visit 0 0
    robot_turn=false
    for i in $input; do
        if $robot_turn; then
            move_robot $i
            visit $rx $ry
            robot_turn=false
        else
            move_santa $i
            visit $sx $sy
            robot_turn=true
        fi
    done
    ls /tmp/day3 | wc -l
    >/dev/null popd
}


input=$(fold -b1 $1)
prepare; part1
prepare; part2
rm -rf "$td"
