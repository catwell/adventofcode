local function round(v: number, sn: number): number
    return (v * sn) % 20201227
end

local function hack(pk1: number, pk2: number): number
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
    for i = 1, c do n = round(n, pk2) end
    return n
end

print(hack(2959251, 4542595))
