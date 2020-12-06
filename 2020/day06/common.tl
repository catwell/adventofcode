local util = require "util"

local M = {}

function M.process(count: function(string): number): number
    local groups = util.split(util.read_file("input.txt"), "\n\n")
    local counts = util.map(groups, count)
    local c = 0
    for _, n in ipairs(counts) do
        c = c + n
    end
    return c
end

return M
