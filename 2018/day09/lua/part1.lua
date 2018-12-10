local util = require "util"
local common = require "common"

local players_count, last_marble = common.parse_input(util.read_file(arg[1]))
local game = common.new_game(players_count, last_marble)
game:play()
print(game:high_score())
