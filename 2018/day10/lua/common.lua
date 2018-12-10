local M = {}

function M.parse_lines(lines)
    local ptrn =
        "position=<([-%s%d]+),([-%s%d]+)> velocity=<([-%s%d]+),([-%s%d]+)>"
    local r = {}
    for i, l in ipairs(lines) do
        local x, y, vx, vy = string.match(l, ptrn)
        r[i] = {
            x = tonumber(x), y = tonumber(y),
            vx = tonumber(vx), vy = tonumber(vy),
        }
    end
    return r
end

local function get_boundaries(points)
    local r = {xmin = math.huge, xmax = 0, ymin = math.huge, ymax = 0}
    for _, p in ipairs(points) do
        if p.x < r.xmin then r.xmin = p.x end
        if p.x > r.xmax then r.xmax = p.x end
        if p.y < r.ymin then r.ymin = p.y end
        if p.y > r.ymax then r.ymax = p.y end
    end
    return r
end

function M.get_area(points)
    local boundaries = get_boundaries(points)
    return (
        math.abs(boundaries.xmax - boundaries.xmin) *
        math.abs(boundaries.ymax - boundaries.ymin)
    )
end

function M.step(points, n)
    for _, p in ipairs(points) do
        p.x = p.x + p.vx * n
        p.y = p.y + p.vy * n
    end
end

local function whitespace_line(n)
    local t = {}
    for i = 1, n do t[i] = " " end
    return t
end

local function shift_pos(boundaries, p)
    return p.x - boundaries.xmin + 1, p.y - boundaries.ymin + 1
end

function M.print_message(points)
    local boundaries = get_boundaries(points)
    local g = {}
    local xmax, ymax = shift_pos(
        boundaries, {x = boundaries.xmax, y = boundaries.ymax}
    )
    for y = 1, ymax do
        g[y] = whitespace_line(xmax)
    end
    for _, p in ipairs(points) do
        local x, y = shift_pos(boundaries, p)
        g[y][x] = "#"
    end
    for y = 1, ymax do
        g[y] = table.concat(g[y])
    end
    print(table.concat(g, "\n"))
end

return M
