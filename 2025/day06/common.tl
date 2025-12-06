local util = require "util"

local M = {}

local record Problem
    op: string
    ints: {integer}
end

local function solve(p: Problem): integer
    if p.op == "+" then
        return util.sum(p.ints)
    elseif p.op == "*" then
        return util.product(p.ints)
    else
        error("invalid op " .. p.op)
    end
end

local function split_line(s: string): {string}
    local r = {}
    local f = function(x: string) r[#r+1] = x end
    s:gsub("(%S+)", f)
    return r
end

local function parse_input_1(input_fn: string): {Problem}
    local lines = util.read_lines(input_fn)
    local split_lines = util.map(lines, split_line)
    local n = #split_lines[1]
    for _, l in ipairs(split_lines) do
        assert(#l == n)
    end
    local r = {}
    for col = 1, n do
        local ints = {}
        for row = 1, #lines - 1 do
            ints[row] = math.tointeger(split_lines[row][col])
        end
        r[col] = {op = split_lines[#lines][col], ints = ints}
    end
    return r
end

local function len(s: string): integer
    return #s
end

local function parse_input_2(input_fn: string): {Problem}
    local lines = util.read_lines(input_fn)
    local ops = split_line(lines[#lines])
    local max_length = util.max(util.map(lines, len))
    -- pad all lines to max size + 1 so there is a blank column at the end
    for i = 1, #lines - 1 do
        while #lines[i] < max_length + 1 do
            lines[i] = lines[i] .. " "
        end
    end
    local r: {Problem} = {}
    local ints: {integer} = {}
    for p = 1, max_length + 1 do
        local t: {string} = {}
        for i = 1, #lines - 1 do
            t[i] = lines[i]:sub(p, p)
        end
        local int = tonumber(table.concat(t))
        if int then
            ints[#ints+1] = math.tointeger(int)
        else
            r[#r+1] = {op = ops[#r+1], ints = ints}
            ints = {}
        end
    end
    assert(#r == #ops)
    return r
end

function M.run(input_fn: string, part: integer) : integer
    local parse_input = parse_input_1
    if part == 2 then
        parse_input = parse_input_2
    end
    local problems = parse_input(input_fn)
    return util.sum(util.map(problems, solve))
end

return M
