local M = {}

-- Note: the value we use for tools is the one of the terrain
-- we *cannot* use the tool on.

local geo_index

function M.parse_input(lines)
    local state = {geo = {[0] = {[0] = 0}}}
    state.depth = assert(tonumber(lines[1]:match("(%d+)")))
    local sx, sy = lines[2]:match("(%d+),(%d+)")
    state.target = {x = assert(tonumber(sx)), y = assert(tonumber(sy))}
    state.geo[state.target.y] = {
        [0] = state.target.y * 48271,
        [state.target.x] = 0,
    }
    -- part 2
    state.done = {["0,0,1"] = 0}
    state.pos = { {x = 0, y = 0, tool = 1, m = 0} }
    state.minute = 0
    return state
end


local function erosion(state, x, y)
    return (geo_index(state, x, y) + state.depth) % 20183
end

geo_index = function(state, x, y)
    if not state.geo[y] then
        state.geo[y] = {[0] = y * 48271 }
    end
    if not state.geo[y][x] then
        if y == 0 then
            state.geo[0][x] = x * 16807
        else
            state.geo[y][x] = erosion(state, x-1, y) * erosion(state, x, y-1)
        end
    end
    return state.geo[y][x]
end

function M.risk_level(state, x, y)
    return erosion(state, x, y) % 3
end

local function done_marker(x, y, tool)
    return string.format("%d,%d,%d", x, y, tool)
end

local function accessible(state, x, y, tool)
    local i, t = 0, {{x, y - 1}, {x - 1, y}, {x + 1, y}, {x, y + 1}}
    local function n()
        i = i + 1
        if t[i] then
            if (
                t[i][1] >= 0 and t[i][2] >= 0 and
                M.risk_level(state, t[i][1], t[i][2]) ~= tool
            ) then
                return t[i][1], t[i][2]
            else
                return n()
            end
        end
    end
    return n
end

function M.tick(state)
    state.minute = state.minute + 1
    local pos = {}
    local function append(x, y, tool, m)
        table.insert(pos, {x = x, y = y, tool = tool, m = m})
    end
    for _, p in ipairs(state.pos) do
        if p.m == 0 then
            for nx, ny in accessible(state, p.x, p.y, p.tool) do
                if nx == state.target.x and ny == state.target.y then
                    if p.tool == 1 then -- torch
                        state.found = true
                        return
                    end
                else
                    local dm = done_marker(nx, ny, p.tool)
                    if not state.done[dm] or state.done[dm] > 0 then
                        state.done[dm] = 0
                        append(nx, ny, p.tool, 0)
                    end
                end
            end
            for tool = 0, 2 do
                local dm = done_marker(p.x, p.y, tool)
                if (
                    tool ~= p.tool and
                    tool ~= M.risk_level(state, p.x, p.y) and
                    (not state.done[dm])
                ) then
                    state.done[dm] = 6
                    append(p.x, p.y, tool, 6)
                end
            end
        else
            local dm = done_marker(p.x, p.y, p.tool)
            state.done[dm] = p.m - 1
            append(p.x, p.y, p.tool, p.m - 1)
        end
    end
    state.pos = pos
end

-- for debug

function M.repr(state)
    local s = { [0] = ".", [1] = "=", [2] = "|"}
    local t = {}
    for y = 0, state.target.y do
        local r = {}
        for x = 0, state.target.x do
            r[x + 1] = s[M.risk_level(state, x, y)]
        end
        t[y + 1] = table.concat(r)
    end
    return table.concat(t, "\n")
end

return M
