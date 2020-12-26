local common = require "common"
local util = require "util"

local tiles = common.parse_input()
local b_map = common.border_to_id(tiles)
local ids = common.corner_ids(tiles, b_map)
print(util.product(ids))
