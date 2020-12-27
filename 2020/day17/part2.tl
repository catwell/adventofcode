local common = require "common"

local st = common.parse_input(true)
for _ = 1, 6 do st = st:step() end
print(st:count_active())
