local c = 0

local function lsort(w)
    local t = {}
    for l in w:gmatch("%w") do
        t[#t+1] = l
    end
    table.sort(t)
    return table.concat(t)
end

for l in io.lines(arg[1]) do
    local t = {}
    for w in l:gmatch("%w+") do
        w = lsort(w)
        if t[w] then goto continue end
        t[w] = true
    end
    c = c + 1
    ::continue::
end

print(c)
