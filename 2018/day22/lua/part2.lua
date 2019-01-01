local util = require "util"
local common = require "common"

local state = common.parse_input(util.read_lines(arg[1]))

while not state.found do
    common.tick(state)
end

print(state.minute)
