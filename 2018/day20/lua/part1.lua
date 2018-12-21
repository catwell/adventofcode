local util = require "util"
local common = require "common"

local input = util.read_file(arg[1])
local regex = common.parse_regexp(input)
print(common.longest_path(regex, common.new_state()))
