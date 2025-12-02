local util = require "util"
local fmt = string.format

local M = {}

local record Range
    start: integer
    stop: integer
end

local function num_digits(x: integer): integer
    return #fmt("%d", x)
end

local function fix_range(r_times: integer): function(Range): {Range}
    return function(range: Range): {Range}
        local l_start, l_stop = num_digits(range.start), num_digits(range.stop)
        assert(math.abs(l_start - l_stop) <= 1)
        if l_start == l_stop then
            return l_start % r_times == 0 and {range} or {}
        elseif l_start % r_times == 0 then
            return {{start = range.start, stop = math.tointeger(10 ^ l_start) -1}}
        elseif l_stop % r_times == 0 then
            return {{start = math.tointeger(10 ^ (l_stop - 1)), stop = range.stop}}
        else
            return {}
        end
    end
end

local function find_invalids(r_times: integer): function(Range): {integer}
    return function(range: Range): {integer}
        local nd = num_digits(range.start)
        assert(num_digits(range.stop) == nd)
        if nd % r_times ~= 0 then return {} end
        local f = math.tointeger(10 ^ (nd // r_times))
        local x = range.start // math.tointeger(10 ^ (nd - nd // r_times))
        local r = {}
        while true do
            local t, f2 = 0, 1
            for _ = 1, r_times do
                t = t + f2 * x
                f2 = f2 * f
            end
            if t > range.stop then break end
            if t >= range.start then r[#r+1] = t end
            x = x + 1
        end
        return r
    end
end

local function parse_input(s: string) : {Range}
    local r: {Range} = {}
    local f = function(start: string, stop: string)
        r[#r+1] = {start=math.tointeger(start), stop=math.tointeger(stop)}
    end
    s:gsub("(%d+)%-(%d+)", f)
    return r
end

function M.run(input_fn: string, part: integer) : integer
    local s = util.read_file(input_fn)
    local input = parse_input(s)

    if part == 1 then
        local ranges = util.flatmap(input, fix_range(2))
        local invalids = util.flatmap(ranges, find_invalids(2))
        return util.sum(invalids)
    end

    local m = 2
    for _, r in ipairs(input) do
        m = math.max(m, num_digits(r.stop))
    end
    local invalids: {{integer:boolean}} = {}
    for i, range in ipairs(input) do
        invalids[i] = {}
        for r_times = 2, m do
            local ranges = fix_range(r_times)(range)
            local invalids_i = util.flatmap(ranges, find_invalids(r_times))
            util.set_add(invalids[i], util.to_set(invalids_i))
        end
    end
    return util.sum(util.map(invalids, util.set_sum))
end

return M
