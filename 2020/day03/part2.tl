local common = require "common"

local input = common.parse_input()

local slopes = {{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}}

local count = 1
for _, slope in ipairs(slopes) do
    count = count * common.count_slope(input, slope[1], slope[2])
end

print(count)
