local util = require "util"

local type Color = string

local record BagData
   count: integer
   color: Color
end

local type ColorSet = {Color:boolean}
local type DirectGraph = {Color:{BagData}}
local type ReverseGraph = {Color:ColorSet}

local M = {
    Color = Color,
    ColorSet = ColorSet,
}

local function parse_line(line: string) : Color, {BagData}
    local color, right = string.match(line, "(%w+ %w+) bags contain (.+)")
    local r = {}
    local f = function(n: string, c: string)
        r[#r+1] = {count = math.tointeger(n), color = c}
    end
    right:gsub("(%d+) (%w+ %w+) bag", f)
    return color, r
end

function M.parse_input() : DirectGraph
    local r = {}
    local lines = util.read_lines("input.txt")
    for _, line in ipairs(lines) do
        local color, data = parse_line(line)
        r[color] = data
    end
    return r as DirectGraph -- inference bug?
end

function M.reverse_graph(g: DirectGraph): ReverseGraph
    local r : ReverseGraph = {}
    for color, data in pairs(g) do
        for _, bd in ipairs(data) do
            if not r[bd.color] then
                r[bd.color] = {}
            end
            r[bd.color][color] = true
        end
    end
    return r
end

return M
