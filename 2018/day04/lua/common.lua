local T = { GUARD = 1, SLEEP = 2, WAKE = 3 }

local function parse_line(line)
    local id = line:match("Guard #(%d+) begins shift")
    if id then return { t = T.GUARD, id = id } end
    local min = assert(tonumber(line:match(":(%d%d)")))
    if line:match("wakes up") then
        return { t = T.WAKE, min = min }
    else
        assert(line:match("falls asleep"))
        return { t = T.SLEEP, min = min }
    end
end

local function process_lines(lines)
    table.sort(lines)
    local sleep_count, sleep_mins = {}, {}
    do
        local guard_id, t_sleep
        for _, line in ipairs(lines) do
            local op = parse_line(line)
            if op.t == T.GUARD then
                guard_id = op.id
            elseif op.t == T.SLEEP then
                t_sleep = op.min
            else
                if not sleep_count[guard_id] then
                    sleep_count[guard_id] = 0
                    sleep_mins[guard_id] = {}
                end
                sleep_count[guard_id] = sleep_count[guard_id] + op.min - t_sleep
                for i = t_sleep, op.min - 1 do
                    sleep_mins[guard_id][i] = (sleep_mins[guard_id][i] or 0) + 1
                end
            end
        end
    end
    return sleep_count, sleep_mins
end

return {
    process_lines = process_lines,
}
