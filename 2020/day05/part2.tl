local common = require "common"

local seat_ids = common.seat_ids()
table.sort(seat_ids)

for i = 2, #seat_ids do
    if seat_ids[i] == seat_ids[i-1] + 2 then
        print(seat_ids[i] - 1)
        return
    end
end
