local util = require "util"

local type Point = {integer, integer}

local record Cell
   risk: integer
   path_risk: integer
   visited: boolean
end

local function parse_line(s: string) : {Cell}
    local t, p = {}, 1
    local function f(x: string)
        t[p] = {
            risk = math.tointeger(x),
            path_risk = math.maxinteger,
            visited = false,
        }
        p = p + 1
    end
    s:gsub("[0-9]", f)
    return t
end

local function parse_input() : {{Cell}}
    local lines = util.read_lines("input.txt")
    return util.map(lines, parse_line)
end

local function neighbors(p: Point): {Point}
    return {
        {p[1] - 1, p[2]},
        {p[1] + 1, p[2]},
        {p[1], p[2] - 1},
        {p[1], p[2] + 1},
    }
end

local function wrap_risk(n: integer): integer
    if n >= 10 then
        return n - 9
    else
        return n
    end
end

local function gen_map2(map: {{Cell}}): {{Cell}}
    local map_rows, map_cols = #map, #map[1]
    local map2 : {{Cell}} = {}

    for row = 1, 5 * map_rows do
        map2[row] = {}
    end

    -- replicate horizontally
    for n = 0, 4 do
        for row = 1, map_rows do
            for i_col = 1, map_cols do
                local o_col = i_col + n * map_cols
                map2[row][o_col] = {
                    risk = wrap_risk(map[row][i_col].risk + n),
                    path_risk = math.maxinteger,
                    visited = false,
                }
            end
        end
    end

    -- replicate vertically
    for n = 1, 4 do
        for i_row = 1, map_rows do
            local o_row = i_row + n * map_rows
            for col = 1, map_cols * 5 do
                map2[o_row][col] = {
                    risk = wrap_risk(map2[i_row][col].risk + n),
                    path_risk = math.maxinteger,
                    visited = false,
                }
            end
        end
    end

    return map2
end

local M = {}

function M.run(part: integer) : integer
    local map = parse_input()
    if part == 2 then
        map = gen_map2(map)
    end
    map[1][1].path_risk = 0
    local border : {Point} = {{1, 1}}
    while #border > 0 do
        local new_border = {}
        for _, p in ipairs(border) do
            local cell = map[p[1]][p[2]]
            for _, np in ipairs(neighbors(p)) do
                local new_cell = (map[np[1]] or {})[np[2]]
                if new_cell then
                    local new_risk = cell.path_risk + new_cell.risk
                    if new_cell.path_risk > new_risk then
                        new_cell.path_risk = new_risk
                        if not new_cell.visited then
                            table.insert(new_border, np)
                            new_cell.visited = true
                        end
                    end
                end
            end
        end
        for _, p in ipairs(new_border) do
            map[p[1]][p[2]].visited = false
        end
        border = new_border
    end
    return map[#map][#map[1]].path_risk
end

return M
