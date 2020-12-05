local common = require "common"

local seat_ids, m = common.seat_ids(), 0
for _, n in ipairs(seat_ids) do
    if n > m then m = n end
end
print(m)
