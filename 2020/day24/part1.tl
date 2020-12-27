local common = require "common"
local util = require "util"

local m = common.get_initial_map()
print(util.count_keys(m as {any: any}))
