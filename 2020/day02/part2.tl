local common = require "common"

local function valid(line: common.LineData): boolean
    local v1 = line.pwd:sub(line.min, line.min) == line.letter
    local v2 = line.pwd:sub(line.max, line.max) == line.letter
    return v1 ~= v2
end

print(common.count_valid(valid))
