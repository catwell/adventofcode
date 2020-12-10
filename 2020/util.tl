local M = {}

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

function M.count_keys(t : {any:any}) : number
    local c = 0
    for _ in pairs(t) do c = c + 1 end
    return c
end

function M.parse_integers(s: string, i0: number) : {number}
    local t, p = {}, i0
    local function f(x: string)
        t[p] = tonumber(x)
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

return M
