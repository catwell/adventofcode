local util = require "util"

local function update(t)
    local l, p = #t, 1
    for i = 2, l do
        if t[i] > t[p] then p = i end
    end
    local c = t[p]
    t[p] = 0
    for i = 1, l do
        t[i] = t[i] + (c // l)
    end
    for i = p + 1, p + (c % l) do
        local j = i % l
        if j == 0 then j = l end
        t[j] = t[j] + 1
    end
end


local t, s = {}, util.read_file(arg[1])

local function f(x) table.insert(t, tonumber(x)) end
s:gsub("%d+", f)

local c, seen, inloop = 0, {}, false

while true do
    s = table.concat(t, " ")
    if seen[s] then
        if inloop then
            print(c)
            return
        else
            c, seen, inloop = 0, {}, true
        end
    end
    update(t)
    c = c + 1
    seen[s] = true
end
