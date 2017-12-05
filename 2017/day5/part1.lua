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
    t[p] = t[p] + 1
    p = p + t[p] - 1
end
