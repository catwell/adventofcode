local util = require "util"

local M = {}

function M.run(input_fn: string, part: integer) : integer
    local s = util.read_file(input_fn)
    s = s:gsub("L", "-"):gsub("R", "")
    local ints = util.parse_integers(s)
    local n, r = 50, 0
    for _, v in ipairs(ints) do
        if part == 1 then
            n = (n + v) % 100
            if n == 0 then r = r + 1 end
        else
            n = n + v
            r = r + math.abs(n // 100)
            -- case where we start from 0 and go left
            if v < 0 and n == v then r = r - 1 end
            n = n % 100
            -- case where we arrive on 0 while going left
            if n == 0 and v < 0 then r = r + 1 end
            -- print(v, n, r)
        end
    end
    return r
end

return M
