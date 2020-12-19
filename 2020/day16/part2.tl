local common = require "common"

local state = common.parse_input()
state:discard_invalid_tickets()
local ticket = state:decoded_ticket()

local c = 1
for field, v in pairs(ticket) do
    if field:match("^departure") then
        c = c * v
    end
end
print(c)
