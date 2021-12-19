local util = require "util"

local type Point = {integer, integer}

local function parse_line(s: string) : {integer}
    local t, p = {9}, 2
    local function f(x: string)
        t[p] = math.tointeger(x)
        p = p + 1
    end
    s:gsub("[0-9]", f)
    t[p] = 9
    return t
end

local function parse_input() : {{integer}}
    -- this adds a border of 9s around the map
    local lines = util.read_lines("input.txt")
    local fill = string.rep('9', #lines[1])
    table.insert(lines, 1, fill)
    table.insert(lines, fill)
    return util.map(lines, parse_line)
end

local function find_lows(map: {{integer}}): {Point}
    local r = {}
    for row = 2, #map - 1 do
        for col = 2, #map[1] - 1 do
            local v = map[row][col]
            local top = map[row - 1][col]
            local bottom = map[row + 1][col]
            local left = map[row][col - 1]
            local right = map[row][col + 1]
            if (
                v < top and
                v < bottom and
                v < left and
                v < right
            ) then
                r[#r+1] = {row, col}
            end
        end
    end
    return r
end

local function part1(map: {{integer}}, lows: {Point}): integer
    local r = 0
    for _, low in ipairs(lows) do
        r = r + map[low[1]][low[2]] + 1
    end
    return r
end

local function serial(p: Point): integer
    return p[1] * 10000 + p[2]
end

local function neighbors(p: Point): {Point}
    return {
        {p[1] - 1, p[2]},
        {p[1] + 1, p[2]},
        {p[1], p[2] - 1},
        {p[1], p[2] + 1},
    }
end

local function basin_size(map: {{integer}}, low: Point): integer
    local visited = {[serial(low)] = true}
    local border = {low}
    local n = 1
    while #border > 0 do
        local new_border = {}
        for _, p in ipairs(border) do
            for _, np in ipairs(neighbors(p)) do
                if map[np[1]][np[2]] ~= 9 and (not visited[serial(np)]) then
                    visited[serial(np)] = true
                    table.insert(new_border, np)
                    n = n + 1
                end
            end
        end
        border = new_border
    end
    return n
end

local function part2(map: {{integer}}, lows: {Point}): integer
    local top3 = {0, 0, 0}
    for _, low in ipairs(lows) do
        local s = basin_size(map, low)
        if s > top3[1] then
            top3[1] = s
            table.sort(top3)
        end
    end
    return top3[1] * top3[2] * top3[3]
end

-- 439

local M = {}

function M.run(part: integer) : integer
    local map = parse_input()
    local lows = find_lows(map)
    if part == 1 then
        return part1(map, lows)
    else
        return part2(map, lows)
    end
end

return M
