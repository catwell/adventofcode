local util = require "util"

local record Line
    x1: integer
    y1: integer
    x2: integer
    y2: integer
end

local function parse_line(line: string) : Line
    local ptrn = "(%d+),(%d+) %-> (%d+),(%d+)"
    local x1, y1, x2, y2 = string.match(line, ptrn)
    return {
        x1 = math.tointeger(x1),
        y1 = math.tointeger(y1),
        x2 = math.tointeger(x2),
        y2 = math.tointeger(y2),
    }
end

local function parse_input() : {Line}
    return util.map(util.read_lines("input.txt"), parse_line)
end


local function make_map(lines: {Line}, with_diag: boolean): util.matrix.Matrix<integer>
    local r = util.matrix.new(0)
    for _, line in ipairs(lines) do
        local x1, x2, y1, y2 = line.x1, line.x2, line.y1, line.y2
        if x1 == x2 then
            if y1 > y2 then y1, y2 = y2, y1 end
            for y = y1, y2 do
                r:set(x1, y, r:get(x1, y) + 1)
            end
        elseif y1 == y2 then
            if x1 > x2 then x1, x2 = x2, x1 end
            for x = x1, x2 do
                r:set(x, y1, r:get(x, y1) + 1)
            end
        elseif with_diag then
            local p = {x = x1, y = y1}
            while true do
                r:set(p.x, p.y, r:get(p.x, p.y) + 1)
                if p.x == x2 then break end
                if x1 < x2 then
                    p.x = p.x + 1
                else
                    p.x = p.x - 1
                end
                if y1 < y2 then
                    p.y = p.y + 1
                else
                    p.y = p.y - 1
                end
            end
        end
    end
    return r
end


local M = {}

function M.run(part: integer) : integer
    local lines = parse_input()
    local map = make_map(lines, part == 2)
    local n = 0
    for row = 0, map.max_row do
        for col = 0, map.max_col do
            if map:get(row, col) > 1 then
                n = n + 1
            end
        end
    end
    return n
end

return M
