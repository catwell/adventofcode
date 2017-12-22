local S = {}

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

local function load()
    for l in io.lines(arg[1]) do
        S[parse_line(l)] = true
    end
end

local function set_size()
    local c = 0
    for _ in pairs(S) do
        c = c + 1
    end
    return c
end

local function increase(d, s)
    d.x = d.x + s.x
    d.y = d.y + s.y
    d.z = d.z + s.z
end

local function tick()
    for p in pairs(S) do
        increase(p.v, p.a)
        increase(p.p, p.v)
    end
end

local function ser_p(p)
    return string.format("%d,%d,%d", p.p.x, p.p.y, p.p.z)
end

local function remove_collisions()
    local s = {}
    for p in pairs(S) do
        local x = ser_p(p)
        s[x] = (s[x] or 0) + 1
    end
    for p in pairs(S) do
        if s[ser_p(p)] > 1 then
            S[p] = nil
        end
    end
end

load()

for _=1, 1000 do -- more than enough
    tick()
    remove_collisions()
end

print(set_size())
