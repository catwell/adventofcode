local util = require "util"

local M = {}

local function parse_input() : {integer}
    return util.map(util.read_lines("input.txt"), math.tointeger as function(string): integer)
end

function M.initial_arrangement(): {integer}
    local t = parse_input()
    table.sort(t)
    table.insert(t, 1, 0)
    table.insert(t, t[#t] + 3)
    return t
end

return M
