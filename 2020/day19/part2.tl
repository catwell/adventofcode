local common = require "common"

local rules, messages = common.parse_input()
common.fix_rules(rules)
print(common.count_matching(rules, messages))
