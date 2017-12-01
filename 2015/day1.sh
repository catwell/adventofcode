c=0; n=1
for i in $(fold -b1 $1); do
    [ "$i" = "(" ] && c=$((c+1))
    [ "$i" = ")" ] && c=$((c-1))
    echo $c $n
    n=$((n+1))
done

# sh day1.sh input.txt | tail -n1 | cut -d' ' -f1
# sh day1.sh input.txt | grep -e '-1 ' | head -n1 | cut -d' ' -f2
