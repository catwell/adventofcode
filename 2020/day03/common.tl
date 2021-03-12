local util = require "util"

local M = {}

local function parse_line(line: string) : {boolean}
    local r, p = {}, 1
    local function f(x: string)
        r[p] = (x == '#')
        p = p + 1
    end
    line:gsub("[#.]", f)
    return r
end

local function make_infinite(row: {boolean}) : {boolean}
    local l = #row
    return setmetatable(row, {
        __index = function(t: {boolean}, i: integer) : boolean
            return rawget(t, ((i - 1) % l) + 1)
        end
    })
end

function M.parse_input() : {{boolean}}
    local lines, r = util.read_lines("input.txt"), {}
    for i = 1, #lines do
        r[i] = make_infinite(parse_line(lines[i]))
    end
    return r
end

function M.display(input: {{boolean}})
    for i = 1, #input do
        local r = input[i]
        for j = 1, #r do
            io.write(r[j] and "#" or '.')
        end
        io.write("\n")
    end
end

function M.count_slope(input: {{boolean}}, right: integer, down: integer) : integer
    local row, col, count = 1, 1, 0
    while input[row] do
        if input[row][col] then count = count + 1 end
        row, col = row + down, col + right
    end
    return count
end

return M
