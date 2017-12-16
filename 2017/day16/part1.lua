local util = require "util"

local STATE = { -- indexed from 1
    "a", "b", "c", "d", "e", "f", "g", "h",
    "i", "j", "k", "l", "m", "n", "o", "p",
}

local function spin(c)
    for _ = 1, c do
        table.insert(STATE, 1, table.remove(STATE))
    end
end

local function find(n)
    for i = 1, #STATE do
        if STATE[i] == n then return i end
    end
end

local function exchange(p1, p2)
    STATE[p1], STATE[p2] = STATE[p2], STATE[p1]
end

local function partner(n1, n2)
    exchange(find(n1), find(n2))
end

local function process(op)
    local optype = op:sub(1, 1)
    if optype == "s" then
        spin(tonumber(op:match("s(%d+)")))
    elseif optype == "x" then
        local p1, p2 = op:match("x(%d+)%/(%d+)")
        exchange(tonumber(p1) + 1, tonumber(p2) + 1)
    elseif optype == "p" then
        partner(op:match("p(%w+)%/(%w+)"))
    end
end

util.read_file(arg[1]):gsub("[^,]+", process)
print(table.concat(STATE))
