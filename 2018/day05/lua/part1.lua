local util = require "util"
local common = require "common"

local input = util.read_file(arg[1])

local t = common.to_byte_table(input)

print(common.react(t))
