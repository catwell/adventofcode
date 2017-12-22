local L = {}

local function parse_triplet(s)
    local x, y, z = s:match("([%d-]+),([%d-]+),([%d-]+)")
    x, y, z = tonumber(x), tonumber(y), tonumber(z)
    return {x = x, y = y, z = z}
end

local function parse_line(l)
    local p, v, a = l:match("p=<([0-9,-]+)>, v=<([0-9,-]+)>, a=<([0-9,-]+)>")
    return {
        p = parse_triplet(p),
        v = parse_triplet(v),
        a = parse_triplet(a),
    }
end

local function manhattan(v)
    return math.abs(v.x) + math.abs(v.y) + math.abs(v.z)
end

local function absolute_acceleration(p)
    return manhattan(p.a)
end

local function load()
    local id = 0
    for l in io.lines(arg[1]) do
        local p = parse_line(l)
        p.id = id
        L[id + 1] = p
        id = id + 1
    end
end

load()

local function cmp(a, b)
    return absolute_acceleration(a) < absolute_acceleration(b)
end
table.sort(L, cmp)
print(L[1].id)
