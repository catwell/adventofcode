local M = {}

local function parse_state(s)
    local p, r = 0, {min = 0}
    local function f(x)
        r[p] = (x == "#")
        p = p + 1
    end
    s:gsub("[#.]", f)
    r.max = p - 1
    return r
end

local function code_at(s, p)
    local r = 0
    if s[p - 2] then r = r + 1 end
    if s[p - 1] then r = r + 2 end
    if s[p] then r = r + 4 end
    if s[p + 1] then r = r + 8 end
    if s[p + 2] then r = r + 16 end
    return r
end

local function encode_rule(rule)
    return code_at(parse_state(rule), 2)
end

local function parse_rules(lines)
    local r = {}
    for i = 3, #lines do
        local rule = lines[i]:match("([.#]+) => #")
        if rule then r[encode_rule(rule)] = true end
    end
    return r
end

local function repr_gen(g)
    local r = {}
    for i = g.min, g.max do
        table.insert(r, g[i] and "#" or ".")
    end
    return table.concat(r)
end

local function score(g)
    local r = 0
    for p = g.min, g.max do
        if g[p] then r = r + p end
    end
    return r
end

local function generate(g, rules)
    local r = {min = g.min - 2, max = g.max + 2}
    for p = r.min, r.max do
        r[p] = rules[code_at(g, p)]
    end
    while not r[r.min] do r.min = r.min + 1 end
    while not r[r.max] do r.max = r.max - 1 end
    return r
end

function M.solve(lines, gen_max)

    local state = parse_state(lines[1])
    local rules = parse_rules(lines)

    local prev_gen_repr = repr_gen(state)
    for i = 1, gen_max do
        local new_state = generate(state, rules, i)

        -- After a while, the state repeats and so
        -- the score increases in a linear fashion.
        local cur_gen_repr = repr_gen(new_state)
        if prev_gen_repr == cur_gen_repr then
            local score_diff = score(new_state) - score(state)
            print(score(new_state) + score_diff * (gen_max - i))
            return
        end

        prev_gen_repr, state = cur_gen_repr, new_state
    end

    -- If the linear state was not reached.
    return score(state)
end

return M
