local util = require "util"
local common = require "common"

local state = common.parse_input(util.read_lines(arg[1]))
common.run(state)

print(state.water_count)

-- local r = common.ppm_repr(state)
-- local f = io.open("image.ppm", "wb")
-- f:write(r)
-- f:close()
