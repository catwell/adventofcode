local n = tonumber(arg[1])
local STATE = { 0 }

local l, p = 1, 0
for i = 1, 2017 do
    p = (p + n) % l + 1
    table.insert(STATE, p + 1, i)
    l = l + 1
end

print(STATE[p + 2])
