local t, m = {}, 0
for l in io.lines(arg[1]) do
    local depth, range = l:match("(%d+): (%d+)")
    depth, range = tonumber(depth), tonumber(range)
    t[depth] = range
    if depth > m then m = depth end
end

local delay = 0
while true do
    for depth = 0, m do
        if t[depth] then
            local d = (t[depth] - 1) * 2
            if (depth + delay) % d == 0 then
                delay = delay + 1
                goto continue
            end
        end
    end
    print(delay)
    do return end
    ::continue::
end
