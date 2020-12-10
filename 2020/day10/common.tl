local util = require "util"

local M = {}

local function parse_input() : {number}
    return util.map(util.read_lines("input.txt"), tonumber as function(string): number)
end

function M.initial_arrangement(): {number}
    local t = parse_input()
    table.sort(t)
    table.insert(t, 1, 0)
    table.insert(t, t[#t] + 3)
    return t
end

return M
