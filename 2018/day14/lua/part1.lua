local util = require "util"
local common = require "common"

local function run(c)
    local state = common.initial_state()
    while state.n <= c + 9 do common.tick(state) end
    local r = {}
    for i = 1, 10 do
        r[i] = tostring(state.s[c + i - 1])
    end
    return table.concat(r)
end

print(run(tonumber(util.read_file(arg[1]))))
