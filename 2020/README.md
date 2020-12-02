# AoC 2020

## Notes

- I don't expect to finish or do most days on time this year for personal reasons.
- Let's do it in [Teal](https://github.com/teal-language/tl), because it's more fun.

## How to run

I use [localua](https://github.com/oploadk/localua) and a local version of `tl` so that I can fix bugs in it as I go if needed.

Setting `TL_PATH` is necessary to find the `util.tl`; setting `package.path` from inside a script won't work with Teal.

```sh
tl_path="/path/to/tl"
curl https://loadk.com/localua.sh -O
sh localua.sh .lua
./.lua/bin/luarocks install tl --only-deps
tl () { TL_PATH='./?.lua;../?.lua' ../.lua/bin/lua "$tl_path" "$@" ; }
cd day01
tl run part1.tl
```
