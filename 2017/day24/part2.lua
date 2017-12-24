local parts = {}

local function new_map()
    return setmetatable(
        {}, {
            __index = function(t, k)
                t[k] = {}
                return t[k]
            end
        }
    )
end

local function copy_table(t)
    local r = {}
    for i = 1, #t do r[i] = t[i] end
    return r
end

local function copy_map(m)
    local r = new_map()
    for k, v in pairs(m) do
        if #v > 0 then
            r[k] = copy_table(v)
        end
    end
    return r
end

local MAP = new_map()

local function prepare()
    for l in io.lines(arg[1]) do
        local x, y = l:match("(%d+)/(%d+)")
        if x then
            x, y = tonumber(x), tonumber(y)
            table.insert(parts, {x = x, y = y})
        end
    end
    for i = 1, #parts do
        parts[i].id = i
        table.insert(MAP[parts[i].x], i)
        table.insert(MAP[parts[i].y], i)
    end
end

local function remove_value(t, v)
    for i = 1, #t do
        if t[i] == v then
            table.remove(t, i)
            return
        end
    end
    error()
end

local lmax, max = 0, 0

local step

local function _step(prev, map, score, len, pos)
    local i = table.remove(map[prev], pos)
    local v = (parts[i].x == prev) and parts[i].y or parts[i].x
    assert(#map[v] > 0)
    remove_value(map[v], i)
    step(v, map, score + prev + v, len + 1)
end

step = function(prev, map, score, len)
    if #map[prev] == 0 then
        if len > lmax then
            lmax, max = len, score
        elseif len == lmax and score > max then
            max = score
        end
        return
    else
        for pos = 2, #map[prev] do
            _step(prev, copy_map(map), score, len, pos)
        end
        _step(prev, map, score, len, 1)
    end
end

prepare()
step(0, MAP, 0, 0)
print(max)
