local common = require "common"

local entries = common.parse_input()
local allergizing_foods = common.get_allergizing_foods(entries)
local c = 0
for _, entry in ipairs(entries) do
    for food in pairs(entry.foods) do
        if not allergizing_foods[food] then
            c = c + 1
        end
    end
end
print(c)
