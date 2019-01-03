local util = require "util"
local common = require "common"

local state = common.parse_input(util.read_lines(arg[1]))

while not state.game_over do
    common.tick(state)
end
print(common.units_remaining(state))
