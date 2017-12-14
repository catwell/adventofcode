-- from day 10

local L_SZ = 256

local function append_standard_lengths(t)
    local x = {17, 31, 73, 47, 23}
    for i = 1, #x do table.insert(t, x[i]) end
end

local function compute_sparse_hash(t)
    local CTX = { pos = 0, ssz = 0 }
    local L = {} -- indexed from 0
    for i = 0, L_SZ - 1 do L[i] = i end

    local function reverse(p, n)
        local c = n // 2
        for i = 0, c - 1 do
            local x1, x2 = (p + i) % L_SZ, (p + n - i - 1) % L_SZ
            L[x1], L[x2] = L[x2], L[x1]
        end
    end

    local function process(n)
        reverse(CTX.pos, n)
        CTX.pos = (CTX.pos + n + CTX.ssz) % L_SZ
        CTX.ssz = CTX.ssz + 1
    end

    for _ = 1, 64 do
        for i = 1, #t do process(t[i]) end
    end

    return L
end

local function compute_dense_hash(sh)
    local t = {}
    for i = 0, 15 do
        local p = 16 * i
        local n = sh[p]
        for j = 1, 15 do n = n ~ sh[p + j] end
        t[i + 1] = n
    end
    return t
end

local hash = function(s)
    local t = {s:byte(1, -1)}
    append_standard_lengths(t)
    local sh = compute_sparse_hash(t)
    local dh = compute_dense_hash(sh)
    return string.char(table.unpack(dh))
end

return { hash = hash }
