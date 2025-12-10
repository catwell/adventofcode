local util = require "util"

local M = {}

local record Point
    x: integer
    y: integer
    z: integer
end

local function parse_input(input_fn: string): {Point}
    local lines = util.read_lines(input_fn)
    local r = {}
    for i, l in ipairs(lines) do
        local x, y = l:match("(%d+)%,(%d+)")
        r[i] = {
            x = math.tointeger(tonumber(x)),
            y = math.tointeger(tonumber(y)),
        }
    end
    return r
end

local record Box
    left: integer
    right: integer
    top: integer
    bottom: integer
end

local function lrtb(p1: Point, p2: Point): Box
    return {
        left = math.min(p1.x, p2.x),
        right = math.max(p1.x, p2.x),
        top = math.min(p1.y, p2.y),
        bottom = math.max(p1.y, p2.y),
    }
end

local function segment_inside(p1: Point, p2: Point, points: {Point}): boolean
    local rect = lrtb(p1, p2)
    for i = 1, #points do
        local sp1, sp2 = points[i], points[i+1]
        if not sp2 then sp2 = points[1] end
        local seg = lrtb(sp1, sp2)
        if (
            seg.top == seg.bottom and
            seg.top > rect.top and seg.top < rect.bottom and
            (
                (seg.left <= rect.left and seg.right > rect.left) or
                (seg.left < rect.right and seg.right >= rect.right)
            )
        ) then return true end
        if (
            seg.left == seg.right and
            seg.left > rect.left and seg.left < rect.right and
            (
                (seg.top <= rect.top and seg.bottom > rect.top) or
                (seg.top < rect.bottom and seg.bottom >= rect.bottom)
            )
        ) then return true end
    end
    return false
end

local function max_area(points: {Point}, part2: boolean): integer
    local m = 0
    for i = 1, #points - 1 do
        local p1 = points[i]
        for j = i, #points do
            local p2 = points[j]
            local a = (math.abs(p1.x - p2.x) + 1) * (math.abs(p1.y - p2.y ) + 1)
            if a > m then
                if (not part2) or (not segment_inside(p1, p2, points)) then
                    m = math.tointeger(a)
                end
            end
        end
    end
    return m
end

function M.run(input_fn: string, part: integer) : integer
    local points = parse_input(input_fn)
    return max_area(points, part == 2)
end

return M
