local util = require "util"
local common = require "common"

local state = common.parse_input(util.read_lines(arg[1]))
common.run(state)

print(state.still_count)
