local common = require "common"

local function cost(input: {integer}, pos: integer): integer
    local r = 0
    for _, v in ipairs(input) do
        local n = math.abs(pos - v)
        r = r + n*(n+1)//2
    end
    return r
end

local function bisect(input: {integer}, pos_l: integer, pos_r: integer): integer
    local cost_l = cost(input, pos_l)
    local cost_r = cost(input, pos_r)
    if pos_r <= pos_l + 1 then
        return math.min(cost_l, cost_r)
    end
    local med = pos_l + (pos_r - pos_l) // 2
    if cost_l < cost_r then
        return bisect(input, pos_l, med)
    else
        return bisect(input, med, pos_r)
    end
end

local input = common.parse_input()
print(bisect(input, 1, #input))
