local util = require "util"
local common = require "common"

local coords = common.parse_lines(util.read_lines(arg[1]))
local boundaries = common.get_grid_bondaries(coords)

local function distance(c, n)
    return math.abs(c.x - n.x) + math.abs(c.y - n.y)
end

local function distance_sum(c)
    local r = 0
    for _, n in ipairs(coords) do
        r = r + distance(c, n)
    end
    return r
end

local max_dist, area = 10000, 0

for x = boundaries.xmin, boundaries.xmax do
    for y = boundaries.ymin, boundaries.ymax do
        if distance_sum({x = x, y = y}) < max_dist then
            area = area + 1
        end
    end
end

print(area)
