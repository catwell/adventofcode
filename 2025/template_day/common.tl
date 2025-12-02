local util = require "util"

local M = {}

function M.run(input_fn: string, part: integer) : integer
    if part == 2 then return 0 end
    local lines = util.read_lines(input_fn)
    return #lines
end

return M
