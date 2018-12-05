local M = {}

function M.to_byte_table(s)
    local t = {}
    local function f(x) table.insert(t, string.byte(x)) end
    s:gsub("%w", f)
    return t
end

local cZ, cz = string.byte("Zz", 1, -1)
local delta = cz - cZ

function M.opposite(c)
    if c > cZ then
        return c - delta
    else
        return c + delta
    end
end

-- We loop backwards because it significantly improves the performance
-- of the table.remove() calls. Ideally it would use a linked list
-- but I am too lazy to implement that for AoC. :)
function M.react(t, n, l)
    if not l then l = #t end
    if not n then n = l - 1 end
    for i = n, 1, -1 do
        if t[i] == M.opposite(t[i+1]) then
            table.remove(t, i)
            table.remove(t, i)
            return M.react(t, math.min(i + 1, l - 3), l - 2)
        end
    end
    return l
end

return M
