local util = require "util"

local record State
    edges: {string}
    couples: {string: integer}
    rules: {string: {string}}
    tick: function(State)
end

local function state_tick(state: State)
    local couples = state.couples
    local next_couples: {string: integer}  = {}
    for couple, count in pairs(couples) do
        if state.rules[couple] then
            for _, p in ipairs(state.rules[couple]) do
                next_couples[p] = (next_couples[p] or 0) + count
            end
        else
            next_couples[couple] = (next_couples[couple] or 0) + count
        end
    end
    state.couples = next_couples
end

local function parse_input() : State
    local lines = util.read_lines("input.txt")
    local template = table.remove(lines, 1)
    local couples: {string: integer}  = {}
    local n = #template
    for i = 1, n - 1 do
        local couple = template:sub(i, i + 1)
        couples[couple] = (couples[couple] or 0) + 1
    end

    local rules = {}
    for _, line in ipairs(lines) do
        local l, r, m = line:match("^([A-Z])([A-Z]) %-> ([A-Z])$")
        if m then rules[l .. r] = {l .. m, m .. r} end
    end

    local edges = {template:sub(1, 1), template:sub(n, n)}

    return {
        couples = couples,
        rules = rules,
        edges = edges,
        tick = state_tick,
    }
end

local M = {}

function M.run(part: integer) : integer
    local state = parse_input()

    for _ = 1, (part == 1) and 10 or 40 do
        state:tick()
    end

    local counts : {string: integer} = {}
    for couple, count in pairs(state.couples) do
        local p1 = couple:sub(1, 1)
        local p2 = couple:sub(2, 2)
        counts[p1] = (counts[p1] or 0) + count
        counts[p2] = (counts[p2] or 0) + count
    end
    for l, v in pairs(counts) do
        -- Every atom is counted twice except edges.
        if l == state.edges[1] then v = v + 1 end
        if l == state.edges[2] then v = v + 1 end
        counts[l] = v // 2
    end
    local min, max = math.maxinteger, 0
    for _, v in pairs(counts) do
        if v < min then
            min = v
        elseif v > max then
            max = v
        end
    end
    return max - min
end

return M
