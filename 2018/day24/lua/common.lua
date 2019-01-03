local util = require "util"

local M = {}

local function _list(s)
    if not s then return {} end
    local v1, v2 = s:match("(%w+), (%w+)")
    return v2 and {[v1] = true, [v2] = true} or {[s] = true}
end

function M.parse_input(lines)
    local r, army = {groups = {}, infection = 0, immune = 0}, "immune"
    for _, l in ipairs(lines) do
        if l:match("Infection") then
            army = "infection"
        else
            local t = {army = army}
            t.units, t.hp, t.dmg, t.initiative =
                table.unpack(util.parse_integers(l, 1))
            if t.initiative then
                t.dmg_type = l:match("(%w+) damage")
                t.weaknesses = _list(l:match("weak to ([^;)]+)"))
                t.immunities = _list(l:match("immune to ([^;)]+)"))
                r[army] = r[army] + 1
                table.insert(r.groups, t)
            end
        end
    end
    return r
end

local function effective_power(g)
    return g.units * g.dmg
end

local function selection_cmp(state)
    return function(a, b)
        local ga, gb = state.groups[a], state.groups[b]
        local ea, eb = effective_power(ga), effective_power(gb)
        return (ea == eb) and (ga.initiative > gb.initiative) or (ea > eb)
    end
end

local function attack_cmp(state)
    return function(a, b)
        local ga, gb = state.groups[a], state.groups[b]
        return ga.initiative > gb.initiative
    end
end

local function sorted_group_list(state, cmp)
    local r = {}
    for k in pairs(state.groups) do
        table.insert(r, k)
    end
    table.sort(r, cmp)
    return r
end

local function tmp_group_map(state)
    local r = {}
    for k in pairs(state.groups) do
        r[k] = {}
    end
    return r
end

local function group_list_for_selection(state)
    return sorted_group_list(state, selection_cmp(state))
end

local function group_list_for_attack(state)
    return sorted_group_list(state, attack_cmp(state))
end

local function attack_damage(g1, g2)
    if g2.immunities[g1.dmg_type] then
        return 0
    elseif g2.weaknesses[g1.dmg_type] then
        return 2 * effective_power(g1)
    else
        return effective_power(g1)
    end
end

local function target_selection(state)
    local r = tmp_group_map(state)
    local gs = group_list_for_selection(state)
    for _, ag in ipairs(gs) do
        local av = state.groups[ag]
        local sg, sv, sdmg
        for tg, tv in pairs(state.groups) do
            if av.army ~= tv.army and (not r[tg].targeted) then
                local tdmg = attack_damage(av, tv)
                if sg then
                    if tdmg > sdmg then
                        sg, sv, sdmg = tg, tv, tdmg
                    elseif tdmg == sdmg then
                        local tp, sp = effective_power(tv), effective_power(sv)
                        if (
                            tp > sp or
                            (tp == sp and tv.initiative > sv.initiative)
                        ) then
                            sg, sv, sdmg = tg, tv, tdmg
                        end
                    end
                else
                    sg, sv, sdmg = tg, tv, tdmg
                end
            end
        end
        if sdmg and sdmg > 0 then
            r[ag].target, r[sg].targeted = sg, ag
        end
    end
    return r
end

function M.tick(state)
    local targets = target_selection(state)
    local gs = group_list_for_attack(state)
    local total_dead = 0
    for _, ag in ipairs(gs) do
        local tg = targets[ag].target or 0
        local av, tv = state.groups[ag], state.groups[tg]
        if av and tv then
            local dmg = attack_damage(av, tv)
            local dead = math.min(tv.units, dmg // tv.hp)
            total_dead = total_dead + dead
            tv.units = tv.units - dead
            if tv.units == 0 then
                state[tv.army] = state[tv.army] - 1
                if state[tv.army] == 0 then
                    state.game_over = true
                end
                state.groups[tg] = nil
            end
        end
    end
    if total_dead == 0 then
        -- draw
        state.game_over = true
    end
end

function M.units_remaining(state)
    local c = 0
    for _, v in pairs(state.groups) do
        c = c + v.units
    end
    return c
end

function M.boost(state, n)
    for _, v in pairs(state.groups) do
        if v.army == "immune" then
            v.dmg = v.dmg + n
        end
    end
end

return M
