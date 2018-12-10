local util = require "util"
local common = require "common"

local points = common.parse_lines(util.read_lines(arg[1]))

common.step(points, 10000) -- Not necessary, but speeds things up.

-- It wasn't obvious but the criterion for the message to appear
-- is apparently to have the minimum area of the bounding box of the points.

local prev_area = math.huge
while true do
    common.step(points, 1)
    local area = common.get_area(points)
    if area > prev_area then
        common.step(points, -1)
        common.print_message(points)
        return
    end
    prev_area = area
end
