local util = require "util"
local common = require "common"

local lines = util.read_lines(arg[1])
print(common.solve(lines, 50000000000))
