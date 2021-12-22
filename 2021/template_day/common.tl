local util = require "util"

local M = {}

function M.run(part: integer) : integer
    if part == 2 then return 0 end
    local lines = util.read_lines("input.txt")
    return #lines
end

return M
