local util = require "util"
local common = require "common"

local lines = util.read_lines(arg[1])

local sleep_count, sleep_mins = common.process_lines(lines)

local guard_most_asleep
do
    local max_sleep = 0
    for guard_id, mins in pairs(sleep_count) do
        if mins > max_sleep then
            guard_most_asleep, max_sleep = guard_id, mins
        end
    end
end

local most_slept_minute
do
    local max_sleep = 0
    for min, t in pairs(sleep_mins[guard_most_asleep]) do
        if t > max_sleep then
            most_slept_minute, max_sleep = min, t
        end
    end
end

print(math.floor(guard_most_asleep * most_slept_minute))
