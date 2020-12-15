-- Notes regardig input: the first ID is not an x; all IDs are prime.

local util = require "util"

local lines = util.read_lines("input.txt")
local buses = util.parse_integers((lines[2]:gsub("x", "0")))

local ts, inc = 0, buses[1]
for i = 2, #buses do
    local bus = buses[i]
    if bus == 0 then goto continue end
    while (ts + i - 1) % bus ~= 0 do
        ts = ts + inc
    end
    inc = inc * bus
    ::continue::
end

print(ts)
