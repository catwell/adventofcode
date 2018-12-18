local M = {}

local function parse_line(l)
    local r, p = {}, 1
    local function f(x)
        r[p] = x
        p = p + 1
    end
    l:gsub(".", f)
    return r
end

function M.parse_input(lines)
    local r = {w = 0, h = 0, map = {}}
    for i, l in ipairs(lines) do
        l = parse_line(l)
        if #l == 0 then break end
        r.h, r.map[i] = i, l
    end
    r.w = #r.map[1]
    r.map[0], r.map[r.h + 1] = {}, {} -- padding
    return r
end

local function new_map(state)
    local r = {}
    for i = 0, state.h + 1 do r[i] = {} end
    return r
end

local function adjacent_count(m, x, y)
    local r = {["."] = 0, ["|"] = 0, ["#"] = 0}
    for _, d in ipairs({
        {-1, -1}, {-1, 0}, {-1, 1},
        {0, -1}, {0, 1},
        {1, -1}, {1, 0}, {1, 1},
    }) do
        local v = m[y + d[1]][x + d[2]]
        if v then r[v] = r[v] + 1 end
    end
    return r
end

local function next_value(m, x, y)
    local adj = adjacent_count(m, x, y)
    if m[y][x] == "." then
        return (adj["|"] >= 3) and "|" or "."
    elseif m[y][x] == "|" then
        return (adj["#"] >= 3) and "#" or "|"
    else
        assert(m[y][x] == "#")
        return (adj["#"] > 0 and adj["|"] > 0) and "#" or "."
    end
end

function M.tick(state, n)
    for _ = 1, n do
        local m = new_map(state)
        for y = 1, state.h do
            for x = 1, state.w do
                m[y][x] = next_value(state.map, x, y)
            end
        end
        state.map = m
    end
end

function M.resource_value(state)
    local r = {["."] = 0, ["|"] = 0, ["#"] = 0}
    for y = 1, state.h do
        for x = 1, state.w do
            local v = state.map[y][x]
            r[v] = r[v] + 1
        end
    end
    return r["|"] * r["#"]
end

function M.repr(state)
    local r = {}
    for i = 1, state.h do
        r[i] = table.concat(state.map[i], "", 1, state.w)
    end
    return table.concat(r, "\n")
end

return M
