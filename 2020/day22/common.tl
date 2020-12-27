local util = require "util"

local M = {}

function M.parse_input(): {number}, {number}
    local t = util.split(util.read_file("input.txt"), "\n\n")
    local s1 = t[1]:match("^Player %d:(.+)$")
    local s2 = t[2]:match("^Player %d:(.+)$")
    return util.parse_integers(s1), util.parse_integers(s2)
end

function M.score(p: {number}): number
    local l, r = #p, 0
    for i = 1, l do
        r = r + i * p[l - i + 1]
    end
    return r
end

return M
