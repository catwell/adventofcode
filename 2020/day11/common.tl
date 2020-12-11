local util = require "util"

local enum Cell
    "."
    "L"
    "#"
end

local type Grid = {{Cell}}

local type SeatCounter = function(Grid, number, number, number): number

local M = {Grid = Grid}

local function parse_line(l: string) : {Cell}
    local r = {}
    local function f(x: string)
        r[#r+1] = x as Cell
    end
    l:gsub("[.#L]", f)
    return r
end

local function parse_input() : Grid
    local lines = util.read_lines("input.txt")
    return util.map(lines, parse_line)
end

local function step(st: Grid, counter: SeatCounter, min_seats: number): Grid, number
    local mod = 0
    local r: Grid = {}
    for row = 1, #st do
        r[row] = {}
        for col = 1, #st[row] do
            local cur = st[row][col]
            local seats = counter(st, row, col, min_seats)
            if cur == "L" and seats == 0 then
                r[row][col] = "#"
                mod = mod + 1
            elseif cur == "#" and seats >= min_seats then
                r[row][col] = "L"
                mod = mod + 1
            else
                r[row][col] = cur
            end
        end
    end
    return r, mod
end

local function count_occupied(st: Grid): number
    local r = 0
    for row = 1, #st do
        for col = 1, #st[row] do
            if st[row][col] == "#" then
                r = r + 1
            end
        end
    end
    return r
end

local function find_fix_point(st: Grid, counter: SeatCounter, min_seats: number): Grid
    local mod = 0
    while true do
        st, mod = step(st, counter, min_seats)
        if mod == 0 then break end
    end
    return st
end

function M.process(counter: SeatCounter, min_seats: number): number
    return count_occupied(find_fix_point(parse_input(), counter, min_seats))
end

return M
