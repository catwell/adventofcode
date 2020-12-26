local common = require "common"

local tiles = common.parse_input()
local b_map = common.border_to_id(tiles)
local grid = common.get_tile_grid(tiles, b_map)
local image = common.get_image(grid)
print(common.water_roughness(image))
