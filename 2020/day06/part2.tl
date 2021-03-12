local common = require "common"
local util = require "util"

local function count(s: string): integer
    local rows = util.split(s, "\n")
    local h : {string:integer} = {}
    local function f(x: string)
        h[x] = (h[x] or 0) + 1
    end
    for _, row in ipairs(rows) do
        row:gsub("[a-z]", f)
    end
    local n, c = #rows, 0
    for _, v in pairs(h) do
        if v == n then
            c = c + 1
        end
    end
    return c
end

print(common.process(count))
