local fmt = string.format

local w

local binary = function(v1, op, v2, d)
    if w[d] then return true end
    if not (w[v1] and w[v2]) then return false end
    if op == "AND" then
        w[d] = (w[v1] & w[v2]) & 0xffff
    elseif op == "OR" then
        w[d] = (w[v1] | w[v2]) & 0xffff
    elseif op == "LSHIFT" then
        w[d] = (w[v1] << w[v2]) & 0xffff
    elseif op == "RSHIFT" then
        w[d] = (w[v1] >> w[v2]) & 0xffff
    else
        error(fmt("unknown operand %s"))
    end
    return true
end

local unary = function(op, v, d)
    assert(op == "NOT")
    if w[d] then return true end
    if not w[v] then return false end
    w[d] = (~w[v]) & 0xffff
    return true
end

local assign = function(v, d)
    if w[d] then return true end
    if not w[v] then return false end
    w[d] = w[v]
    return true
end

local expr = {
    {"^([a-z0-9]+) ([A-Z]+) ([a-z0-9]+) %-> ([a-z]+)$", binary},
    {"^([A-Z]+) ([a-z0-9]+) %-> ([a-z]+)$", unary},
    {"^([a-z0-9]+) %-> ([a-z]+)$", assign},
}

local prefilled = function()
    local r = {}
    for i=0,0xffff do r[tostring(i)] = i end
    return r
end

local interp = function(s)
    local t
    for i=1,#expr do
        t = {s:match(expr[i][1])}
        if t[1] then
            return expr[i][2](unpack(t))
        end
    end
    if s == "" then return true end
    error(fmt("could not interpret %s", s))
end

local interp_all = function(lines)
    local c = 1
    while next(lines) do
        print(fmt("round %d (%d)", c, #lines))
        local next_lines = {}
        for i=1,#lines do
            if not interp(lines[i]) then
                next_lines[#next_lines + 1] = lines[i]
            end
        end
        lines = next_lines
        c = c + 1
    end
end

local copy = function(t)
    local r = {}
    for i=1,#t do r[i] = t[i] end
    return r
end

local f = assert(io.open(arg[1]))
local lines = {}
for x in f:lines() do lines[#lines+1] = x end
f:close()

w = prefilled()
interp_all(copy(lines))
print("part 1: ", w.a)

local r_a = w.a
w = prefilled()
w.b = r_a
interp_all(lines)
print("part 2: ", w.a)
