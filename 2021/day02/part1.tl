local common = require "common"

local pos, depth = 0, 0
local input = common.parse_input()

for _, move in ipairs(input) do
    if move.direction == "forward" then
        pos = pos + move.units
    elseif move.direction == "up" then
        depth = depth - move.units
    else
        depth = depth + move.units
    end
end

print(pos * depth)
