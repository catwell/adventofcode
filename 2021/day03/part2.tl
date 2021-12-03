local common = require "common"

local enum BitS
    "0"
    "1"
end

local type FilterFun = function(count: integer, total: integer): BitS

local function step(lines: {string}, pos: integer, filter_fun: FilterFun): {string}
    local count = common.get_count(lines, pos)
    local filter_val = filter_fun(count, #lines)
    local r = {}
    for _, line in ipairs(lines) do
        if line:sub(pos, pos) == filter_val then
            table.insert(r, line)
        end
    end
    return r
end

local function to_decimal(s: string): integer
    local m, r = 1, 0
    for i = #s, 1, -1 do
        if s:sub(i, i) == "1" then
            r = r + m
        end
        m = m * 2
    end
    return r
end

local function find_rating(filter_fun: FilterFun): integer
    local lines = common.parse_input()
    local pos = 1
    while #lines > 1 do
        lines = step(lines, pos, filter_fun)
        pos = pos + 1
    end
    return to_decimal(lines[1])
end

local filter_oxy = function(count: integer, total: integer): BitS
    return count >= (total - count) and "1" or "0"
end

local filter_co2 = function(count: integer, total: integer): BitS
    return count < (total - count) and "1" or "0"
end

print(find_rating(filter_oxy) * find_rating(filter_co2))
