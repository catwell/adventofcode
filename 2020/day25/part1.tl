local function round(v: integer, sn: integer): integer
    return (v * sn) % 20201227
end

local function hack(pk1: integer, pk2: integer): integer
    local n, c = 1, 0
    while true do
        n, c = round(n, 7), c + 1
        if n == pk1 then
            break
        elseif n == pk2 then
            pk2 = pk1
            break
        end

    end
    n = 1
    for _ = 1, c do n = round(n, pk2) end
    return n
end

print(hack(2959251, 4542595))
