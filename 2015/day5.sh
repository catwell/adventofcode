enough_vowels () { [ "$(echo -n "$1" | sed 's/[^aeiou]//g' | wc -c)" -ge 3 ] ; }
has_duplicates () { ! [ "$(echo -n "$1" | tr -s -c '')" == "$1" ] ; }
has_bad_parts () { echo -n "$1" | grep 'ab\|cd\|pq\|xy' >/dev/null ; }
nice1 () { enough_vowels "$1" && has_duplicates "$1" && ! has_bad_parts "$1" ; }

repeating_pair () { echo -n "$1" | grep -E '(..).*\1' >/dev/null ; }
split_pair () { echo -n "$1" | grep -E '(.).\1' >/dev/null ; }
nice2 () { repeating_pair "$1" && split_pair "$1" ; }

c1=0; c2=0
for i in $(cat $1); do
    nice1 "$i" && c1=$((c1+1))
    nice2 "$i" && c2=$((c2+1))
done
echo $c1
echo $c2
