local util = require "util"
local common = require "common"

local S = common.parse_input(util.read_lines(arg[1]))

while true do
    local c = common.tick(S, false)
    if c then
        print(string.format("%d,%d", c.x, c.y))
        return
    end
end
