local common = require "common"

local state = common.new_state(common.INPUT, 1000000)
for _ = 1, 10000000 do common.move(state) end
print(state.cups[1] * state.cups[state.cups[1]])
