local M = {}

local function at(s, pos)
    return s:sub(pos, pos)
end

local parse_disj

local function parse_conj(s, pos)
    local r, disj = {t = "conj"}
    while true do
        local p = s:find("[^%w]", pos)
        for i = pos, p - 1 do
            table.insert(r, at(s, i))
        end
        if at(s, p) == "(" then
            disj, p = parse_disj(s, p)
            table.insert(r, disj)
            pos = p + 1
        else
            return r, p
        end
    end
end

parse_disj = function(s, pos)
    assert(at(s, pos) == "(")
    local r, conj = {t = "disj"}
    pos = pos + 1
    while true do
        conj, pos = parse_conj(s, pos)
        table.insert(r, conj)
        if at(s, pos) == "|" then
            pos = pos + 1
        else
            assert(at(s, pos) == ")")
            return r, pos
        end
    end
end

function M.parse_regexp(s)
    s = "(" .. s:match("%^([^%^%$]+)%$") .. ")"
    return parse_disj(s, 1)
end

function M.new_state()
    return {
        x = 0, y = 0, turn = 0,
        visited_last = { [0] = {[0] = 0}},
        visited_first = { [0] = {[0] = 0}},
    }
end

local function copy_state(state)
    local r = {
        x = state.x, y = state.y, turn = state.turn,
        visited_last = {},
        visited_first = state.visited_first -- same reference
    }
    for y, l in pairs(state.visited_last) do
        r.visited_last[y] = {}
        for x, v in pairs(l) do
            r.visited_last[y][x] = v
        end
    end
    return r
end

local deltas = {
    ["N"] = {0, 1},
    ["S"] = {0, -1},
    ["E"] = {1, 0},
    ["W"] = {-1, 0},
}

local function visit(state, direction)
    local dx, dy = table.unpack(deltas[direction])
        state.x, state.y = state.x + dx, state.y + dy
    if not state.visited_last[state.y] then
        state.visited_last[state.y] = {}
    end
    if not state.visited_first[state.y] then
        state.visited_first[state.y] = {}
    end
    local r = 1
    if state.visited_last[state.y][state.x] then
        r = state.visited_last[state.y][state.x] - state.turn
    end
    state.turn = state.turn + 1
    state.visited_first[state.y][state.x] = math.min(
        state.visited_first[state.y][state.x] or math.huge,
        state.turn
    )
    state.visited_last[state.y][state.x] = state.turn
    return r
end

local function longest_path(t, state)
    if type(t) == "string" then
        return visit(state, t)
    end
    local r = 0
    if t.t == "disj" then
        for _, v in ipairs(t) do
            local s = copy_state(state)
            r = math.max(r, longest_path(v, s))
        end
    else
        for _, v in ipairs(t) do
            r = r + longest_path(v, state)
        end
    end
    return r
end

M.longest_path = longest_path

-- for debug

local repr_disj

local function repr_conj(t)
    local r = {}
    for i, v in ipairs(t) do
        r[i] = repr_disj(v)
    end
    return table.concat(r)
end

repr_disj = function(t)
    if type(t) == "string" then
        return t
    end
    local r = {}
    for i, v in ipairs(t) do
        r[i] = repr_conj(v)
    end
    return "(" .. table.concat(r, "|") .. ")"
end

M.repr_disj = repr_disj

return M
