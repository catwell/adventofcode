local util = require "util"

local s = util.read_file(arg[1])

local n, prev = 0, s:match("%d")

local function p(x)
    if x == prev then n = n + tonumber(x) end
    prev = x
end

s:reverse():gsub("%d", p)

print(n)
