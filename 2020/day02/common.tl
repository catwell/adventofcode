local util = require "util"

local record LineData
   min: number
   max: number
   letter: string
   pwd: string
end

local type ValidChecker = function(LineData): boolean

local M = {LineData = LineData}

local function parse_line(line: string) : LineData
    local ptrn = "(%d+)-(%d+) ([a-z]): ([a-z]+)"
    local min, max, letter, pwd = string.match(line, ptrn)
    return {
        min = tonumber(min),
        max = tonumber(max),
        letter = letter,
        pwd = pwd,
    }
end

local function parse_lines() : {LineData}
    local lines, r = util.read_lines("input.txt"), {}
    for i = 1, #lines do
        r[i] = parse_line(lines[i])
    end
    return r
end

function M.count_valid(valid: ValidChecker) : number
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
