local matrix = require "util.matrix"

local M = {matrix = matrix}

function M.read_file(path: string) : string
    local f = assert(io.open(path, "rb"))
    local r = assert(f:read("a"))
    f:close()
    return r
end

function M.read_lines(path: string) : {string}
    local f = assert(io.open(path, "rb"))
    local r = {}
    while true do
        local l = f:read("l")
        if not l then break end
        table.insert(r, l)
    end
    f:close()
    return r
end

function M.copy_table<K,V>(t: {K:V}) : {K:V}
    local r = {}
    for k, v in pairs(t) do
        r[k] = v
    end
    return r
end

function M.extend_seq<A>(t1: {A}, t2: {A})
    local l1 = #t1
    for i, v in ipairs(t2) do t1[l1 + i] = v end
end

function M.count_keys(t : {any:any}) : integer
    local c = 0
    for _ in pairs(t) do c = c + 1 end
    return c
end

function M.parse_integers(s: string, i0: integer) : {integer}
    local t, p = {}, i0 or 1
    local function f(x: string)
        t[p] = math.tointeger(x)
        p = p + 1
    end
    s:gsub("[-%d]+", f)
    return t
end

function M.map<A,B>(t: {A}, f: function(A):(B)): {B}
    local r = {}
    for i, v in ipairs(t) do
        r[i] = f(v)
    end
    return r
end

function M.count<A>(t: {A}, f: function(A):(boolean)): integer
    local r = 0
    for _, v in ipairs(t) do
        if f(v) then r = r + 1 end
    end
    return r
end

function M.split(s: string, sep: string): {string}
    local r = {}
    for v in (s .. sep):gmatch("(.-)" .. sep) do
        r[#r+1] = v
    end
    return r
end

function M.to_set<A>(t: {A}): {A: boolean}
    local r: {A: boolean} = {}
    for _, v in ipairs(t) do r[v] = true end
    return r
end

function M.set_add<K,V>(s1: {K:V}, s2: {K:V})
    for k, v in pairs(s2) do
        s1[k] = s1[k] or v
    end
end

function M.set_subtract<K,V,W>(s1: {K:V}, s2: {K:W})
    for v in pairs(s2) do s1[v] = nil end
end

function M.set_intersect<K,V,W>(s1: {K:V}, s2: {K:W})
    for v in pairs(s1) do
        if not s2[v] then s1[v] = nil end
    end
end

function M.sum(t: {integer}): integer
    local r = 0
    for _, v in ipairs(t) do r = r + v end
    return r
end

function M.product(t: {integer}): integer
    local r = 1
    for _, v in ipairs(t) do r = r * v end
    return r
end

return M
