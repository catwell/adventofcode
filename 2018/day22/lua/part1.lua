local util = require "util"
local common = require "common"

local state = common.parse_input(util.read_lines(arg[1]))

local total_risk = 0
for y = 0, state.target.y do
    for x = 0, state.target.x do
        total_risk = total_risk + common.risk_level(state, x, y)
    end
end
print(total_risk)
