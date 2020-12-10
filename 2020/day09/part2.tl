local common = require "common"

local function get_solution(input: {number}, p0: number, p1: number): number
    local min, max = input[p0], input[p0]
    for i = p0 + 1, p1 do
        if input[i] < min then min = input[i] end
        if input[i] > max then max = input[i] end
    end
    return min + max
end

local input = common.parse_input()
local match = common.find_invalid(input)

for p = 1, #input do
    local s = input[p]
    if s ~= match then
        local i = p + 1
        while input[i] and s < match do
            s = s + input[i]
            if s == match then
                print(get_solution(input, p, i))
                return
            end
            i = i + 1
        end
    end
end
