# Terribly slow because I call md5sum for *every* attempt.
# But well, that's Bash for you :)

key="$1"

n=1
while true; do
    [ "$(echo -n "$key$n" | md5sum | head -c 5)" = "00000" ] && break
    n=$((n+1))
done
echo $n

while true; do
    [ "$(echo -n "$key$n" | md5sum | head -c 6)" = "000000" ] && break
    n=$((n+1))
done
echo $n
