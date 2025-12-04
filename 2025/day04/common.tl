local util = require "util"

local M = {}

local S = 25


local record Point
   row: integer
   col: integer
end

local function neighbors(p: Point): {Point}
    return {
        {row = p.row - 1, col = p.col - 1},
        {row = p.row, col = p.col - 1},
        {row = p.row + 1, col = p.col - 1},
        {row = p.row - 1, col = p.col},
        {row = p.row + 1, col = p.col},
        {row = p.row - 1, col = p.col + 1},
        {row = p.row, col = p.col + 1},
        {row = p.row + 1, col = p.col + 1},
    }
end

local type Map = {integer:boolean}

local function parse_input(input_fn: string): Map, integer, integer
    local r: Map = {}
    local lines = util.read_lines(input_fn)
    local width, height = 0, 0
    for row, v in ipairs(lines) do
        width = #v
        for col = 1, #v do
            if v:sub(col, col) == "@" then
                r[(row << S) + col] = true
            end
        end
        height = row
    end
    return r, width, height
end


local function step(map: Map, width:integer, height: integer): Map, integer
    local r, s = {}, 0
    for row = 1, height do
        for col = 1, width do
            local offset = (row << S) + col
            if map[offset] then
                local n = 0
                for _, p in ipairs(neighbors({row = row, col = col})) do
                    if map[(p.row << S) + p.col] then n = n + 1 end
                end
                if n < 4 then
                    s = s + 1
                else
                    r[offset] = true
                end
            end
        end
    end
    return r, s
end

function M.run(input_fn: string, part: integer) : integer
    local map, width, height = parse_input(input_fn)
    if part == 1 then
        local _, s = step(map, width, height)
        return s
    end
    local s = 0
    while true do
        local x: integer
        map, x = step(map, width, height)
        s = s + x
        if x == 0 then return s end
    end
end

return M
