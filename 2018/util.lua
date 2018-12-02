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

return M
