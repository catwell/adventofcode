local util = require "util"
local common = require "common"

local serial = tonumber(util.read_file(arg[1]))

local G = common.integral_image(serial)

local pow_max, x_max, y_max = -math.huge, 0, 0
for y = 1, 300 - 2 do
    for x = 1, 300 - 2 do
        local p = common.square_power(G, x, y, 3)
        if p > pow_max then
            pow_max, x_max, y_max = p, x, y
        end
    end
end

print(string.format("%d,%d", x_max, y_max))
