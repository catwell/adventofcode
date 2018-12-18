local util = require "util"
local common = require "common"

local state = common.parse_input(util.read_lines(arg[1]))

local n, former = 0, {}
while true do
    n = n + 1
    common.tick(state, 1)
    local r = common.repr(state)
    if former[r] then
        local period = n - former[r]
        local remaining = (1000000000 - n) % period
        common.tick(state, remaining)
        break
    end
    former[r] = n
end

print(common.resource_value(state))
