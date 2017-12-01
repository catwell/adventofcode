smallest () { echo -e "$1\n$2\n$3" | sort -n | head -n1 ; }
second () { echo -e "$1\n$2\n$3" | sort -n | head -n2 | tail -n1 ; }

paper () {
    local a=$(($1*$2))
    local b=$(($2*$3))
    local c=$(($3*$1))
    local s=$(smallest $a $b $c)
    echo $(( 2*(a+b+c) + s ))
}

ribbon () {
    echo $(( 2*($(smallest $1 $2 $3) + $(second $1 $2 $3)) + $1*$2*$3 ))
}

t_paper=0
t_ribbon=0
package () {
    local x=$(echo $1 | cut -dx -f1)
    local y=$(echo $1 | cut -dx -f2)
    local z=$(echo $1 | cut -dx -f3)
    t_paper=$(( $t_paper + $(paper $x $y $z) ))
    t_ribbon=$(( $t_ribbon + $(ribbon $x $y $z) ))
}

for i in $(cat $1); do
    package $i
done

echo $t_paper
echo $t_ribbon
