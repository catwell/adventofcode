local util = require "util"
local common = require "common"

local lines = util.read_lines(arg[1])

local power = 4
while true do
    local state = common.parse_input(lines)
    state.attack_power.E = power
    while not (state.an_elf_died or state.game_over) do
        common.tick(state)
    end
    if not state.an_elf_died then
        print(common.score(state))
        return
    end
    power = power + 1
end
