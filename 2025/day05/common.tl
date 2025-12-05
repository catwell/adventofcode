local util = require "util"

local M = {}

local record Range
    start: integer
    stop: integer
end

local function parse_input(input_fn: string): {Range}, {integer}
    local lines = util.read_lines(input_fn)
    local ranges, ids, p2 = {}, {}, false
    for i, s in ipairs(lines) do
        if s == "" then
            p2 = true
        elseif p2 then
            ids[#ids + 1] = math.tointeger(s)
        else
            local r1, r2 = s:match("(%d+)%-(%d+)")
            ranges[i] = {start = math.tointeger(r1), stop = math.tointeger(r2)}
        end
    end
    return ranges, ids
end

local function in_range(r: Range, id: integer): boolean
    return id >= r.start and id <= r.stop
end

local function is_fresh(ranges: {Range}, id: integer): boolean
    for _, r in ipairs(ranges) do
        if in_range(r, id) then return true end
    end
    return false
end

local function intersect(r1: Range, r2: Range): Range | nil
    if in_range(r1, r2.start) or in_range(r1, r2.stop) or in_range(r2, r1.start) then
        return {start=math.min(r1.start, r2.start), stop=math.max(r1.stop, r2.stop)}
    end
    return nil
end

local function merge(ranges: {Range}, r: Range)
    for i, r2 in ipairs(ranges) do
        local m = intersect(r, r2)
        if m then
            table.remove(ranges, i)
            return merge(ranges, m)  -- could intersect again
        end
    end
    ranges[#ranges+1] = r
end

function M.run(input_fn: string, part: integer) : integer
    local ranges, ids = parse_input(input_fn)
    if part == 1 then
        local s = 0
        for _, i in ipairs(ids) do
            if is_fresh(ranges, i) then s = s + 1 end
        end
        return s
    else
        local rs = {}
        for _, r in ipairs(ranges) do
            merge(rs, r)
        end
        local s = 0
        for _, r in ipairs(rs) do
            s = s +  r.stop - r.start + 1
        end
        return s
    end
end

return M
