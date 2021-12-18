local util = require "util"

local function parse_line(s: string) : {string}
    local t, p = {}, 1
    local function f(x: string)
        t[p] = x
        p = p + 1
    end
    s:gsub("[a-g]+", f)
    return t
end

local M = {}

function M.parse_input() : {{string}}
    return util.map(util.read_lines("input.txt"), parse_line)
end

function M.run(part: integer) : integer
    local lines = util.read_lines("input.txt")
    return part * #lines
end

return M
