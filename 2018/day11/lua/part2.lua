local util = require "util"
local common = require "common"

local serial = tonumber(util.read_file(arg[1]))

local G = common.integral_image(serial)

local pow_max, x_max, y_max, s_max = -math.huge, 0, 0, 0
for s = 1, 300 do
    for y = 1, 300 - s + 1 do
        for x = 1, 300 - s + 1 do
            local p = common.square_power(G, x, y, s)
            if p > pow_max then
                pow_max, x_max, y_max, s_max = p, x, y, s
            end
        end
    end
end

print(string.format("%d,%d,%d", x_max, y_max, s_max))
