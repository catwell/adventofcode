local common = require "common"

local function distant_occupied(st: common.Grid, row: integer, col: integer, stop_at: integer): integer
    local n, c, max_row, max_col = 1, 0, #st, #st[1]
    local found = {}
    local dirs = {
        {-1, -1}, {-1, 0}, {-1, 1},
        {0, -1}, {0, 1},
        {1, -1,}, {1, 0}, {1, 1},
    }
    while c < stop_at do
        if (
            row - n < 1 and
            row + n > max_row and
            col - n < 1 and
            col + n > max_col
        ) then break end
        for i, d in ipairs(dirs) do
            if not found[i] then
                local _row, _col = row + d[1] * n, col + d[2] * n
                if _row > 0 and _row <= max_row and st[_row][_col] ~= "." then
                    if st[_row][_col] == "#" then c = c + 1 end
                    found[i] = true
                end
            end
        end
        n = n + 1
    end
    return c
end

print(common.process(distant_occupied, 5))
