local common = require "common"

local g = common.parse_input()
local counts : {common.Color:integer} = {}
local function get_count(color: common.Color): integer
    if counts[color] then
        return counts[color]
    end
    local n = 1
    for _, data in ipairs(g[color]) do
        n = n + data.count * get_count(data.color)
    end
    counts[color] = n
    return n
end
print(get_count("shiny gold") - 1)
