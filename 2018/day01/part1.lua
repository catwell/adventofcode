local util = require "util"

local s = util.read_file(arg[1])

local n = 0

for v in s:gmatch("[+-]%d+") do
    n = n + tonumber(v)
end

print(n)
