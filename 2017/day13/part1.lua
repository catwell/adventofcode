local t, m = {}, 0
for l in io.lines(arg[1]) do
    local depth, range = l:match("(%d+): (%d+)")
    depth, range = tonumber(depth), tonumber(range)
    t[depth] = range
    if depth > m then m = depth end
end

local c = 0
for depth = 0, m do
    if t[depth] then
        local d = (t[depth] - 1) * 2
        if depth % d == 0 then
            c = c + depth * t[depth]
        end
    end
end

print(c)
