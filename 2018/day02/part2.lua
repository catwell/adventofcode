local util = require "util"

local lines = util.read_lines(arg[1])

local len = #lines[1]

local function str_minus(s, i)
    return s:sub(1, i-1) .. s:sub(i+1, len)
end

for i = 1, len do
    local subs = {}
    for _, l in ipairs(lines) do
        local s = str_minus(l, i)
        if subs[s] then
            print(s)
            return
        end
        subs[s] = true
    end
end
