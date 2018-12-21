-- Same as part 1, except we set R0 to the last value of R2
-- that does not repeat.

local r2, found, last = 0, {}
while true do
    local r5 = r2 | 65536
    r2 = 5234604
    while true do
        r2 = (((r2 + (r5 & 255)) & 16777215) * 65899) & 16777215
        if 256 > r5 then
            if found[r2] then
                print(last)
                return
            end
            last, found[r2] = r2, true
            break
        else
            r5 = r5 // 256
        end
    end

end
