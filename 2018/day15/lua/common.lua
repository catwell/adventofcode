local M = {}

local function parse_line(state, y, l)
    local units_r, walls_r = {}, {}
    local p = 1
    local function f(x)
        if x == "#" then
            walls_r[p] = true
        elseif x == "G" then
            local n = #state.goblins + 1
            units_r[p] = {t = x, hp = 200, n = n}
            state.goblins[n] = {x = p, y = y}
        elseif x == "E" then
            local n = #state.elves + 1
            units_r[p] = {t = x, hp = 200, n = n}
            state.elves[n] = {x = p, y = y}
        end
        p = p + 1
    end
    l:gsub("[#.GE]", f)
    state.units[y], state.walls[y] = units_r, walls_r
end

function M.parse_input(lines)
    local state = {
        w = #lines[1], h = #lines,
        units = {}, walls = {},
        goblins = {}, elves = {},
        round = 0,
        attack_power = { G = 3, E = 3 },
    }
    for i = 1, state.h do
        parse_line(state, i, lines[i])
    end
    return state
end

local function neighbors(x, y)
    local i, t = 0, {{x, y - 1}, {x - 1, y}, {x + 1, y}, {x, y + 1}}
    return function()
        i = i + 1
        if t[i] then
            return t[i][1], t[i][2]
        end
    end
end

local function find_target(state, x, y)
    local unit, hp_min = state.units[y][x], math.huge
    for x2, y2 in neighbors(x, y) do
        local u = state.units[y2][x2]
        if u and u.t ~= unit.t then
            if u.hp < hp_min then hp_min = u.hp end
        end
    end
    if hp_min == math.huge then return end
    for x2, y2 in neighbors(x, y) do
        local u = state.units[y2][x2]
        if u and u.t ~= unit.t and u.hp == hp_min then
            return {x = x2, y = y2}
        end
    end
end

local function empty_map(state)
    local r = {}
    for y = 1, state.h do r[y] = {} end
    return r
end

-- Optim: this could be generated *once* and maintained.
local function map_of_squares_in_range(state, l)
    local r = empty_map(state)
    for _, c in pairs(l) do
        for x, y in neighbors(c.x, c.y) do
            if not (state.units[y][x] or state.walls[y][x]) then
                r[y][x] = true
            end
        end
    end
    return r
end

local function reading_order_cmp(a, b)
    return a.y < b.y or (a.y == b.y and a.x < b.x)
end

local function closest_square_in_range(state, start_x, start_y, map)
    local reached, border = empty_map(state), {{x = start_x, y = start_y}}
    reached[start_y][start_x] = true
    while true do
        local in_range, new_border = {}, {}
        for _, p in ipairs(border) do
            for x, y in neighbors(p.x, p.y) do
                if state.walls[y][x] or state.units[y][x] then
                    reached[y][x] = true
                end
                if not reached[y][x] then
                    reached[y][x] = true
                    local first_step = p.first_step or {x = x, y = y}
                    local v = {first_step = first_step, x = x, y = y}
                    if map[y][x] then
                        table.insert(in_range, v)
                    else
                        table.insert(new_border, v)
                    end
                end
            end
        end
        if #in_range > 0 then
            table.sort(in_range, reading_order_cmp)
            return in_range[1]
        end
        if #new_border == 0 then
            return nil
        end
        border = new_border
    end
end

local function get_friends_list(state, unit)
    return (unit.t == "E" and state.elves or state.goblins)
end

local function get_enemy_list(state, unit)
    return (unit.t == "G" and state.elves or state.goblins)
end

local function move_unit(state, x, y, x2, y2)
    local unit = state.units[y][x]
    local self_list = get_friends_list(state, unit)
    self_list[unit.n].x, self_list[unit.n].y = x2, y2
    state.units[y][x], state.units[y2][x2] = nil, unit
end

local function attack(state, attacker, x, y)
    local unit = state.units[y][x]
    unit.hp = unit.hp - state.attack_power[attacker.t]
    if unit.hp <= 0 then
        if unit.t == "E" then
            state.an_elf_died = true
        end
        local self_list = get_friends_list(state, unit)
        self_list[unit.n] = nil
        state.units[y][x] = nil
    end
end

local function play_unit(state, x, y)
    local unit = state.units[y][x]
    if unit.round == state.round then return end
    unit.round = state.round
    local target = find_target(state, x, y)
    if not target then -- move
        local enemy_list = get_enemy_list(state,unit)
        local map = map_of_squares_in_range(state, enemy_list)
        local dest = closest_square_in_range(state, x, y, map)
        if not dest then return end
        local step = dest.first_step
        move_unit(state, x, y, step.x, step.y)
        x, y = step.x, step.y
        target = find_target(state, x, y)
    end
    if target then -- attack
        attack(state, unit, target.x, target.y)
    end
end

function M.tick(state)
    for y = 1, state.h do
        for x = 1, state.w do
            if state.units[y][x] then
                if not next(get_enemy_list(state, state.units[y][x])) then
                    state.game_over = true
                    return
                end
                play_unit(state, x, y)
            end
        end
    end
    state.round = state.round + 1
end

function M.score(state)
    local hp = 0
    for _, u in pairs(state.elves) do
        hp = hp + state.units[u.y][u.x].hp
    end
    for _, u in pairs(state.goblins) do
        hp = hp + state.units[u.y][u.x].hp
    end
    return hp * state.round
end

-- for debug

function M.map_repr(state)
    local r = {}
    for y = 1, state.h do
        local l = {}
        for x = 1, state.w do
            if state.walls[y][x] then
                l[x] = "#"
            else
                local c = state.units[y][x]
                l[x] = c and c.t or " "
            end
        end
        r[y] = table.concat(l)
    end
    return table.concat(r, "\n")
end

return M
