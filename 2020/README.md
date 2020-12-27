# AoC 2020

## Notes

Let's do it in [Teal](https://github.com/teal-language/tl), because it's more fun.

## How to run

I use [localua](https://github.com/oploadk/localua) and a local version of `tl` so that I can fix bugs in it as I go if needed (see `tl.sh`).

```sh
tl_path="/path/to/tl"
curl https://loadk.com/localua.sh -O
sh localua.sh .lua
./.lua/bin/luarocks install tl --only-deps
cd day01
../tl.sh run part1.tl
```
