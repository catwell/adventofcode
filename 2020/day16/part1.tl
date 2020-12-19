local common = require "common"

local state = common.parse_input()
print(state:scanning_error_rate())
