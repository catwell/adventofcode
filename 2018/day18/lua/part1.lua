local util = require "util"
local common = require "common"

local state = common.parse_input(util.read_lines(arg[1]))
common.tick(state, 10)
print(common.resource_value(state))
