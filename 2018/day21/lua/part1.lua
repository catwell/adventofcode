-- Again, I reversed the program and simplified it.
-- See the reverse step in reverse.lua.
-- We just set R0 to the first value of R2 at line 28.

local r5 = 65536
local r2 = 5234604

while true do
    r2 = (((r2 + (r5 & 255)) & 16777215) * 65899) & 16777215
    if 256 > r5 then
        print(r2)
        do return end
    end
    r5 = r5 // 256
end

