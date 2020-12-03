# AoC 2020

## Notes

- I don't expect to finish or do most days on time this year for personal reasons.
- Let's do it in [Teal](https://github.com/teal-language/tl), because it's more fun.

## How to run

I use [localua](https://github.com/oploadk/localua) and a local version of `tl` so that I can fix bugs in it as I go if needed.

```sh
tl_path="/path/to/tl"
curl https://loadk.com/localua.sh -O
sh localua.sh .lua
./.lua/bin/luarocks install tl --only-deps
tl () { ../.lua/bin/lua "$tl_path" --include-dir .. --skip-compat53 "$@" ; }
cd day01
tl run part1.tl
```
