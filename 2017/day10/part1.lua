local util = require "util"

local CTX = { pos = 0, ssz = 0 }

local L_SZ = assert(tonumber(arg[2]))
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

local s = util.read_file(arg[1])
local function f(x) process(tonumber(x)) end
s:gsub("%d+", f)

print(L[0] * L[1])
