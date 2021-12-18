local util = require "util"

local M = {}

function M.parse_input() : {integer}
    local r = util.parse_integers(util.read_file("input.txt"))
    table.sort(r)
    return r
end

return M
