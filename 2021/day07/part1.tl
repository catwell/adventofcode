local common = require "common"

local input = common.parse_input()
local pos = input[#input // 2] -- input has no edge case
local r = 0
for _, v in ipairs(input) do
    r = r + math.abs(pos - v)
end
print(r)
