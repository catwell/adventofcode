local util = require "util"

local enum Direction
    "e"
    "se"
    "sw"
    "w"
    "nw"
    "ne"
end

local M = {}

-- we use odd-r layout, see https://www.redblobgames.com/grids/hexagons/
local direction : {string: {Direction: {number}}} = {
    even = {
        e =  { 1,  0},
        w =  {-1,  0},
        ne = { 0, -1},
        nw = {-1, -1},
        se = { 0,  1},
        sw = {-1,  1},
    },
    odd = {
        e =  { 1,  0},
        w =  {-1,  0},
        ne = { 1, -1},
        nw = { 0, -1},
        se = { 1,  1},
        sw = { 0,  1},
    },
}


local function move(x: number, y: number, dir: Direction): number, number
    local off = direction[y % 2 == 0 and "even" or "odd"][dir]
    return x + off[1], y + off[2]
end

local function move_series(x: number, y: number, series: string): number, number
    if series == "" then return x, y end
    local c1 = series:sub(1, 1)
    if c1 == "e" or c1 == "w" then
        local x2, y2 = move(x, y, c1 as Direction)
        return move_series(x2, y2, series:sub(2))
    else
        local x2, y2 = move(x, y, series:sub(1, 2) as Direction)
        return move_series(x2, y2, series:sub(3))
    end
end

local function make_ref(x: number, y: number): string
    return tostring(x) .. "," .. tostring(y)
end

local function parse_ref(s: string): number, number
    return table.unpack(util.parse_integers(s))
end

local function neighbors(ref: string): {string}
    local x, y = parse_ref(ref)
    local offsets = direction[y % 2 == 0 and "even" or "odd"]
    local r = {}
    for _, off in pairs(offsets) do
        r[#r + 1] = make_ref(x + off[1], y + off[2])
    end
    return r
end

function M.step(m: {string: boolean}): {string: boolean}
    local r : {string: boolean} = {}
    local to_consider : {string: boolean} = {}
    for ref in pairs(m) do
        to_consider[ref] = true
        for _, ref2 in ipairs(neighbors(ref)) do to_consider[ref2] = true end
    end
    for ref in pairs(to_consider) do
        local c = 0
        for _, ref2 in ipairs(neighbors(ref)) do
            if m[ref2] then c = c + 1 end
         end
        if (c == 2 or (m[ref] and c == 1)) then r[ref] = true end
    end
    return r
end

function M.get_initial_map() : {string: boolean}
    local lines = util.read_lines("input.txt")
    local m : {string: boolean} = {}
    for _, line in ipairs(lines) do
        local x, y = move_series(0, 0, line)
        local ref = make_ref(x, y)
        if m[ref] then m[ref] = nil else m[ref] = true end
    end
    return m
end

return M
