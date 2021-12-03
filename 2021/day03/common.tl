local util = require "util"

local M = {}

function M.parse_input(): {string}
    return util.read_lines("input.txt")
end

function M.get_count(lines: {string}, pos: integer): integer
    local r = 0
    for  _, line in ipairs(lines) do
        r = r + math.tointeger(line:sub(pos, pos))
    end
    return r
end

function M.get_counts(lines: {string}): {integer}
    local r : {integer} = {}
    for pos = 1, #lines[1] do
        r[pos] = M.get_count(lines, pos)
    end
    return r
end

return M
