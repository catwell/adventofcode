local util = require "util"

local s = util.read_file(arg[1])

local n, found = 0, {}

while true do
    for v in s:gmatch("[+-]%d+") do
        if found[n] then
            print(n)
            return
        end
        found[n] = true
        n = n + tonumber(v)
    end
end
