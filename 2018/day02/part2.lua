local util = require "util"

local lines = util.read_lines(arg[1])

local len, subs = #lines[1], {}
for i = 1, len do subs[i] = {} end

local function str_minus(s, i)
    return s:sub(1, i-1) .. s:sub(i+1, len)
end

for _, l in ipairs(lines) do
    for i = 1, len do
        local s = str_minus(l, i)
        if subs[i][s] then
            print(s)
            return
        end
        subs[i][s] = true
    end
end
