local common = require "common"

local rules, messages = common.parse_input()
print(common.count_matching(rules, messages))
