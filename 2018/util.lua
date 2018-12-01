local M = {}

function M.read_file(path)
    local f = assert(io.open(path, "rb"))
    local r = assert(f:read("a"))
    f:close()
    return r
end

return M
