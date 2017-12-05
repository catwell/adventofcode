local t = {}

for l in io.lines(arg[1]) do
    table.insert(t, tonumber(l))
end

local c, p = 0, 1

while true do
    if not t[p] then
        print(c)
        return
    end
    c = c + 1
    local v = t[p]
    t[p] = t[p] + (t[p] >= 3 and -1 or 1)
    p = p + v
end
