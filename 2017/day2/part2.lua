local c = 0

for l in io.lines(arg[1]) do
    local t = {}
    local function f(x)
        table.insert(t, tonumber(x))
    end
    l:gsub("%d+", f)
    table.sort(t)
    local found = false
    for i = 2, #t do
        for j = 1, i - 1 do
            if t[i] % t[j] == 0 then
                found = true
                c = c + t[i] // t[j]
                break
            end
        end
        if found then break end
    end
    assert(found)
end

print(c)
