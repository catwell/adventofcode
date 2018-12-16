local M = {}

function M.read_file(path)
    local f = assert(io.open(path, "rb"))
    local r = assert(f:read("a"))
    f:close()
    return r
end

function M.read_lines(path)
    local f = assert(io.open(path, "rb"))
    local r = {}
    while true do
        local l = f:read("l")
        if not l then break end
        table.insert(r, l)
    end
    f:close()
    return r
end

function M.copy_table(t)
    local r = {}
    for k, v in pairs(t) do
        r[k] = v
    end
    return r
end

function M.count_keys(t)
    local c = 0
    for _ in pairs(t) do c = c + 1 end
    return c
end

function M.parse_integers(s, i0)
    local t, p = {}, i0
    local function f(x)
        t[p] = tonumber(x)
        p = p + 1
    end
    s:gsub("%d+", f)
    return t
end

return M
