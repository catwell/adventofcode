# Only question 1. Sloooow. Switched to Lua for performance.

l=1000

pos () { echo $(($2*$l+$1)) ; }

ntimes () { head -c $1 < /dev/zero | tr '\0' $2 ; }

_set () {
    local x=$(($4-$2+1))
    local y=$(($5-$3+1))
    local f=$(ntimes $x $1)
    local p=$(pos $2 $3)
    for i in $(seq 1 $y); do
        s="${s:0:$p}${f}${s:$((p+$x))}"
        p=$((p+l))
    done
}

on () { _set 1 $* ; }
off () { _set 0 $* ; }

toggle () {
    local x=$(($3-$1+1))
    local y=$(($4-$2+1))
    local p=$(pos $1 $2)
    for i in $(seq 1 $y); do
        local f=$(echo "${s:$p:$x}" | tr '01' '10')
        s="${s:0:$p}${f}${s:$((p+$x))}"
        p=$((p+l))
    done
}

process () {
    local x=$(echo "$1" | grep -oE "([0-9]+)")
    local a=$(echo $x | cut -d' ' -f1)
    local b=$(echo $x | cut -d' ' -f2)
    local c=$(echo $x | cut -d' ' -f3)
    local d=$(echo $x | cut -d' ' -f4)
    case $1 in
        *on*) on $a $b $c $d ;;
        *off*) off $a $b $c $d ;;
        *toggle*) toggle $a $b $c $d ;;
    esac
}

s=$(ntimes $((l*l)) 0)

while read i; do
    echo $i
    process "$i"
done < $1

echo -n $s | tr -d '0' | wc -c
