local util = require "util"
local lines = util.read_lines(arg[1])

local function parse_line(line)
    local ptrn = "#(%d+) @ (%d+),(%d+): (%d+)x(%d+)"
    local id, x, y, w, h = string.match(line, ptrn)
    return {id = id, x = x, y = y, w = w, h = h}
end

local map = {}
local map_w = 0

local function enlarge(w)
    if map_w >= w then return end
    for i = map_w + 1, w do
        map[i] = {}
    end
    map_w = w
end

local count = 0

for _, line in ipairs(lines) do
    local l = parse_line(line)
    enlarge(l.x + l.w)
    for i = l.x + 1, l.x + l.w do
        for j = l.y + 1, l.y + l.h do
            map[i][j] = (map[i][j] or 0) + 1
            if map[i][j] == 2 then
                count = count + 1
            end
        end
    end
end

print(count)
