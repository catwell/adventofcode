local util = require "util"
local common = require "common"

local points = common.parse_lines(util.read_lines(arg[1]))

local steps = 10000
common.step(points, steps)

local prev_area = math.huge
while true do
    common.step(points, 1)
    local area = common.get_area(points)
    if area > prev_area then
        print(steps)
        return
    end
    prev_area = area
    steps = steps + 1
end
