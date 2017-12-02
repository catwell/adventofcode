local c = 0

for l in io.lines(arg[1]) do
    local min, max = math.huge, -math.huge
    local function f(x)
        x = tonumber(x)
        if x < min then min = x end
        if x > max then max = x end
    end
    l:gsub("%d+", f)
    if min < max then c = c + max - min end
end

print(c)
