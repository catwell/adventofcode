local c = 0

for l in io.lines(arg[1]) do
    local t = {}
    for w in l:gmatch("%w+") do
        if t[w] then goto continue end
        t[w] = true
    end
    c = c + 1
    ::continue::
end

print(c)
