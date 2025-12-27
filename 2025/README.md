# AoC 2025

## Setup

I use [localua](https://github.com/oploadk/localua) and a local version of `tl` so that I can fix bugs in it as I go if needed (see `tl.sh`).

```bash
curl https://loadk.com/localua.sh -O
sh localua.sh .lua
# use rockspec because deps can be different from prod
./.lua/bin/luarocks install ../../tl/tl-dev-1.rockspec --only-deps
```

## Getting started

```bash
./new_day.sh
./run_last.sh
```
