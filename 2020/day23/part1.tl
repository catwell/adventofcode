local common = require "common"

local state = common.new_state(common.INPUT, 9)
for _ = 1, 100 do common.move(state) end

local function signature(cups: {integer}): string
    local r, p = {}, 1
    while true do
        p = cups[p]
        if p == 1 then break end
        r[#r + 1] = p
    end
    return table.concat(r as {string})
end

print(signature(state.cups))
