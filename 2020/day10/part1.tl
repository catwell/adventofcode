local common = require "common"

local t = common.initial_arrangement()
local n1, n3 = 0, 0
for p = 2, #t do
    local d = t[p] - t[p-1]
    if d == 1 then
        n1 = n1 + 1
    elseif d == 3 then
        n3 = n3 + 1
    end
end
print(n1 * n3)
