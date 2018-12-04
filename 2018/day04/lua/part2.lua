local util = require "util"
local common = require "common"

local lines = util.read_lines(arg[1])

local _, sleep_mins = common.process_lines(lines)

local guard_most_asleep, most_slept_minute
do
    local max_sleep = 0
    for guard_id, mins in pairs(sleep_mins) do
        for min, t in pairs(mins) do
            if t > max_sleep then
                guard_most_asleep, most_slept_minute, max_sleep =
                    guard_id, min, t
            end
        end
    end
end

print(math.floor(guard_most_asleep * most_slept_minute))
