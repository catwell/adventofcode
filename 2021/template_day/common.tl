local util = require "util"

local M = {}

function M.run(part: integer) : integer
    local lines = util.read_lines("input.txt")
    return part * #lines
end

return M
