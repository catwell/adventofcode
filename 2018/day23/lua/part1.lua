local util = require "util"
local common = require "common"

local bots = common.parse_lines(util.read_lines(arg[1]))

local boss = 1
for i = 2, #bots do
    if bots[i].r > bots[boss].r then
        boss = i
    end
end

local c = 0
for i = 1, #bots do
    if common.in_range(bots[boss], bots[i]) then
        c = c + 1
    end
end

print(c)
