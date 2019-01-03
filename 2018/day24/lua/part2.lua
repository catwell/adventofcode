local util = require "util"
local common = require "common"

local lines = util.read_lines(arg[1])

local boost = 0
while true do
    local state = common.parse_input(lines)
    common.boost(state, boost)
    while not state.game_over do
        common.tick(state)
    end
    if state.infection == 0 then
        print(common.units_remaining(state))
        return
    end
    boost = boost + 1
end

