local common = require "common"

local function play(p1: {number}, p2: {number})
    if p1[1] > p2[1] then p1, p2 = p2, p1 end
    table.insert(p2, table.remove(p2, 1))
    table.insert(p2, table.remove(p1, 1))
end

local p1, p2 = common.parse_input()
while #p1 > 0 and #p2 > 0 do play(p1, p2) end
print(common.score(#p1 > 0 and p1 or p2))
