local common = require "common"

local function adjacent_occupied(st: common.Grid, row: integer, col: integer): integer
    local c = 0
    if row > 1 then
        for i = col - 1, col + 1 do
            if st[row - 1][i] == "#" then c = c + 1 end
        end
    end
    if st[row][col - 1] == "#" then c = c + 1 end
    if st[row][col + 1] == "#" then c = c + 1 end
    if row < #st then
        for i = col - 1, col + 1 do
            if st[row + 1][i] == "#" then c = c + 1 end
        end
    end
    return c
end

print(common.process(adjacent_occupied, 4))
