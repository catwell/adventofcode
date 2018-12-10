local util = require "util"
local common = require "common"

local coords = common.parse_lines(util.read_lines(arg[1]))
local grid, boundaries = {}, common.get_grid_boundaries(coords)

for i = boundaries.xmin, boundaries.xmax do
    grid[i] = {}
end

local function neighbors(c)
    return {
        {x = c.x - 1, y = c.y},
        {x = c.x + 1, y = c.y},
        {x = c.x, y = c.y - 1},
        {x = c.x, y = c.y + 1},
    }
end

local function is_outside(c)
    return (
        c.x < boundaries.xmin or c.x > boundaries.xmax or
        c.y < boundaries.ymin or c.y > boundaries.ymax
    )
end

local S, ncoords = {}, #coords
for i = 1, ncoords do
    S[i] = {l = {coords[i]}, area = 0}
    grid[coords[i].x][coords[i].y] = {round = 0, v = i}
end

local round = 1
while true do
    local over = true
    for i = 1, ncoords do
        local next_l = {}
        for _, c in ipairs(S[i].l) do
            if grid[c.x][c.y].v == i then
                S[i].area = S[i].area + 1
                for _, n in ipairs(neighbors(c)) do
                    if is_outside(n) then
                        S[i].infinite = true
                    elseif not grid[n.x][n.y] then
                        grid[n.x][n.y] = {round = round, v = i}
                        table.insert(next_l, n)
                        over = false
                    elseif (
                        grid[n.x][n.y].v ~= i and
                        grid[n.x][n.y].round == round
                    ) then
                        grid[n.x][n.y].v = 0
                    end
                end
            end
        end
        S[i].l = next_l
    end
    if over then break end
    round = round + 1
end

local area_max = 0
for i = 1, ncoords do
    if not S[i].infinite then
        if S[i].area > area_max then
            area_max = S[i].area
        end
    end
end

print(area_max)
