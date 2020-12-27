local common = require "common"

local game : function({number}, {number}): {number}

local function round(p1: {number}, p2: {number}, played: {string: boolean}): {number}
    local sig = table.concat(p1 as {string}, ",") .. "|" .. table.concat(p2 as {string}, ",")
    if played[sig] then
        return p1
    end
    played[sig] = true

    local c1 = table.remove(p1, 1)
    local c2 = table.remove(p2, 1)
    if #p1 < c1 or #p2 < c2 then
        if c1 > c2 then p1, c1, p2, c2 = p2, c2, p1, c1 end
        table.insert(p2, c2)
        table.insert(p2, c1)
    else
        local sub_p1 = {table.unpack(p1, 1, c1)}
        local sub_p2 = {table.unpack(p2, 1, c2)}
        local w = game(sub_p1, sub_p2)
        if w == sub_p1 then p1, c1, p2, c2 = p2, c2, p1, c1 end
        table.insert(p2, c2)
        table.insert(p2, c1)
    end
end

function game(p1: {number}, p2: {number}): {number}
    local played, winner = {}, {}
    while #p1 > 0 and #p2 > 0 do
        winner = round(p1, p2, played)
        if winner then break end
    end
    if not winner then
        winner = #p1 > 0 and p1 or p2
    end
    return winner
end

local p1, p2 = common.parse_input()
print(common.score(game(p1, p2)))
