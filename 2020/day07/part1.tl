local common = require "common"
local util = require "util"

local g = common.reverse_graph(common.parse_input())
local l1, c1 : common.ColorSet, number
local l2, c2 : common.ColorSet, number = {["shiny gold"] = true}, 1
while c1 ~= c2 do
    l1, c1 = l2, c2
    l2 = {}
    for c1 in pairs(l1) do
        l2[c1] = true
        if g[c1] then
            for c2 in pairs(g[c1]) do
                l2[c2] = true
            end
        end
    end
    c2 = util.count_keys(l2 as {any:any})
end
print(c2 - 1)
