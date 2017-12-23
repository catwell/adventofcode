local h = 0

for b = 108100, 125100, 17 do
    for d = 2, b do
        if b % d == 0 then -- loop is useless otherwise
            for e = 2, b do
                if d * e == b then
                    h = h + 1
                    goto continue
                end
            end
        end
    end
    ::continue::
end

print(h)
