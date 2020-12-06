local common = require "common"

local function count(s: string): number
    local h, c = {}, 0
    local function f(x: string)
        if not h[x] then
            c = c + 1
            h[x] = true
        end
    end
    s:gsub("[a-z]", f)
    return c
end

print(common.process(count))
