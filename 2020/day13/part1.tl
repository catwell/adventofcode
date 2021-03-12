local util = require "util"

local lines = util.read_lines("input.txt")
local time, buses = math.tointeger(lines[1]), util.parse_integers(lines[2])

local min, min_bus = math.huge, 0
for _, bus in ipairs(buses) do
    local ts = bus
    while ts < time do ts = ts + bus end
    if ts - time < min then
        min, min_bus = ts - time, bus
    end
end

print(min * min_bus)
