local common = require "common"

local lines = common.parse_input()
local counts = common.get_counts(lines)

local m, gamma, epsilon = 1, 0, 0
for i = #counts, 1, -1 do
    if counts[i] > (#lines - counts[i]) then
        gamma = gamma + m
    else
        epsilon = epsilon + m
    end
    m = m * 2
end

print(gamma * epsilon)
