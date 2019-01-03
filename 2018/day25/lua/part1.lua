local util = require "util"

local function parse_input(lines)
    local r = {}
    for i, l in ipairs(lines) do
        r[i] = util.parse_integers(l, 1)
    end
    return r
end

local function get_dimensions(stars)
    local r = {}
    for c = 1, 4 do
        r[c] = {min = math.huge, max = -math.huge}
    end
    for _, s in ipairs(stars) do
        for c = 1, 4 do
            if s[c] < r[c].min then r[c].min = s[c] end
            if s[c] > r[c].max then r[c].max = s[c] end
        end
    end
    return r
end

local function map_get(map, s)
    return map[s[4]][s[3]][s[2]][s[1]]
end

local function map_set(map, s, v)
    map[s[4]][s[3]][s[2]][s[1]] = v
end

local function make_map(stars)
    local dimensions = get_dimensions(stars)
    local r = {}
    for d4 = dimensions[4].min - 3, dimensions[4].max + 3 do
        r[d4] = {}
        for d3 = dimensions[3].min - 3, dimensions[3].max + 3 do
            r[d4][d3] = {}
            for d2 = dimensions[2].min - 3, dimensions[2].max + 3 do
                r[d4][d3][d2] = {}
            end
        end
    end
    for i, s in ipairs(stars) do
        map_set(r, s, i)
    end
    return r
end

local function norm(s)
    return math.abs(s[1]) + math.abs(s[2]) + math.abs(s[3]) + math.abs(s[4])
end

local function deltas_in_range()
    local r = {}
    for x = -3, 3 do
        for y = -3, 3 do
            for z = -3, 3 do
                for t = -3, 3 do
                    local p = {x, y, z, t}
                    local n = norm(p)
                    if n > 0 and n <= 3 then
                        table.insert(r, p)
                    end
                end
            end
        end
    end
    return r
end

local _deltas = deltas_in_range()

local function in_range(s)
    local i = 0
    return function()
        i = i + 1
        local d = _deltas[i]
        if d then
            return {s[1] + d[1], s[2] + d[2], s[3] + d[3], s[4] + d[4]}
        end
    end
end

local function get_hits(stars)
    local map = make_map(stars)
    local hits = {}
    for i, s in ipairs(stars) do
        hits[i] = {[i] = true}
        for p in in_range(s) do
            local v = map_get(map, p)
            if v then
                hits[i][v] = true
            end
        end
    end
    return hits
end

local function clear(hits, stack)
    while true do
        local v = table.remove(stack)
        if not v then return end
        for k in pairs(v) do
            if hits[k] then
                table.insert(stack, hits[k])
                hits[k] = nil
            end
        end
    end
end

local function get_constellation_count(stars)
    local hits = get_hits(stars)
    local c = 0
    while true do
        local _, v = next(hits)
        if not v then return c end
        clear(hits, {v})
        c = c + 1
    end
end

local stars = parse_input(util.read_lines(arg[1]))
print(get_constellation_count(stars))
