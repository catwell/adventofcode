local common = require "common"

local input = common.parse_input()

local r = 0
for _, line in ipairs(input) do
    for pos = 11, 14 do
        local n = #line[pos]
        if n == 2 or n == 4 or n == 3 or n == 7 then
            r = r + 1
        end
    end
end
print(r)
