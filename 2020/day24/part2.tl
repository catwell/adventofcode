local common = require "common"
local util = require "util"

local m = common.get_initial_map()
for _ = 1, 100 do m = common.step(m) end
print(util.count_keys(m as {any: any}))
