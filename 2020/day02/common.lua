package.path = package.path .. ";../?.lua"
local util = require "util"

local M = {}

local function parse_line(line)
    local ptrn = "(%d+)-(%d+) ([a-z]): ([a-z]+)"
    local min, max, letter, pwd = string.match(line, ptrn)
    return {
        min = tonumber(min),
        max = tonumber(max),
        letter = letter,
        pwd = pwd,
    }
end

local function parse_lines()
    local lines = util.read_lines("input.txt")
    for i = 1, #lines do
        lines[i] = parse_line(lines[i])
    end
    return lines
end

function M.count_valid(valid)
    local lines = parse_lines()
    local n = 0
    for i = 1, #lines do
        if valid(lines[i]) then
            n = n + 1
        end
    end
    return n
end

return M
