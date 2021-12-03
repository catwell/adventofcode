local common = require "common"

local pos, depth, aim = 0, 0, 0
local input = common.parse_input()

for _, move in ipairs(input) do
    if move.direction == "forward" then
        pos = pos + move.units
        depth = depth + aim * move.units
    elseif move.direction == "up" then
        aim = aim - move.units
    else
        aim = aim + move.units
    end
end

print(pos * depth)
