local n = tonumber(arg[1])

local l, p, v = 1, 0, 0

for i = 1, 50000000 do
    p = (p + n) % l + 1
    if p == 1 then v = i end
    l = l + 1
end

print(v)
