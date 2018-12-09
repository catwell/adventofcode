local M = {}

function M.parse_lines(lines)
    local r = {}
    for i, l in ipairs(lines) do
        local x, y = string.match(l, "(%d+), (%d+)")
        r[i] = {x = tonumber(x), y = tonumber(y)}
    end
    return r
end

function M.get_grid_bondaries(coords)
    local r = {xmin = math.huge, xmax = 0, ymin = math.huge, ymax = 0}
    for _, l in ipairs(coords) do
        if l.x < r.xmin then r.xmin = l.x end
        if l.x > r.xmax then r.xmax = l.x end
        if l.y < r.ymin then r.ymin = l.y end
        if l.y > r.ymax then r.ymax = l.y end
    end
    return r
end


return M
