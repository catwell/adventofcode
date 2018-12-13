local M = {}

local intersection_pick = {
    ["<"] = { [0] = "v", [1] = "<", [2] = "^" },
    [">"] = { [0] = "^", [1] = ">", [2] = "v" },
    ["^"] = { [0] = "<", [1] = "^", [2] = ">" },
    ["v"] = { [0] = ">", [1] = "v", [2] = "<" },
}

local turn_pick = {
    ["<"] = { ["\\"] = "^", ["/"] = "v" },
    [">"] = { ["\\"] = "v", ["/"] = "^" },
    ["^"] = { ["\\"] = "<", ["/"] = ">" },
    ["v"] = { ["\\"] = ">", ["/"] = "<" },
}

local function parse_line(state, y, l)
    state.map[y], state.ships[y] = {}, {}
    local p = 0
    local function f(x)
        if turn_pick[x] then -- a ship
            if x == ">" or x == "<" then
                state.map[y][p] = "-"
            elseif x == "^" or x == "v" then
                state.map[y][p] = "|"
            end
            state.ships[y][p] = { v = x, c = 0, t = 0 }
            state.ships_count = state.ships_count + 1
        elseif x ~= " " then
            state.map[y][p] = x
        end
        p = p + 1
    end
    l:gsub(".", f)
    return r
end

function M.parse_input(lines)
    local state = {
        w = 0, h = #lines,
        map = {}, ships = {},
        tick = 0, ships_count = 0,
    }
    for i = 1, state.h do
        local x = #lines[i]
        if x > state.w then state.w = x end
        parse_line(state, i - 1, lines[i])
    end
    return state
end

local function deltas(v)
    local dx = { ["<"] = -1, [">"] = 1, ["^"] = 0, ["v"] = 0 }
    local dy = { ["<"] = 0, [">"] = 0, ["^"] = -1, ["v"] = 1 }
    return dx[v], dy[v]
end

local function move_ship(state, x, y)
    local ship = state.ships[y][x]
    local dx, dy = deltas(ship.v)
    local x2, y2 = x + dx, y + dy
    if state.ships[y2][x2] then -- collision
        return {x = x2, y = y2}
    end
    local next_ship = { v = ship.v, c = ship.c, t = state.tick }
    local floor = state.map[y2][x2]
    if floor == "+" then
        next_ship.v = intersection_pick[ship.v][ship.c % 3]
        next_ship.c = next_ship.c + 1
    elseif floor == "\\" or floor == "/" then
        next_ship.v = turn_pick[ship.v][floor]
    end
    state.ships[y][x] = nil
    state.ships[y2][x2] = next_ship
end

function M.tick(state, die_on_collision)
    state.tick = state.tick + 1
    for y = 0, state.h - 1 do
        for x = 0, state.w - 1 do
            if state.ships[y][x] and state.ships[y][x].t < state.tick then
                local c = move_ship(state, x, y)
                if c then
                    if die_on_collision then
                        return c
                    end
                    state.ships[y][x] = nil
                    state.ships[c.y][c.x] = nil
                    state.ships_count = state.ships_count - 2
                end
            end
        end
    end
    if state.ships_count == 1 then
        for y = 0, state.h - 1 do
            for x = 0, state.w - 1 do
                if state.ships[y][x] then
                    return {x = x, y = y}
                end
            end
        end
    end
end

-- below: debug only

local function line_repr(l, w)
    local r = {}
    for i = 1, w do
        local x = l[i - 1]
        if type(x) == "table" then x = x.v end
        r[i] = x or " "
    end
    return table.concat(r)
end

local function grid_repr(g, w, h)
    local r = {}
    for i = 1, h do
        r[i] = line_repr(g[i - 1], w)
    end
    return table.concat(r, "\n")
end

function M.map_repr(state)
    return grid_repr(state.map, state.w, state.h)
end

function M.ships_repr(state)
    return grid_repr(state.ships, state.w, state.h)
end

return M
