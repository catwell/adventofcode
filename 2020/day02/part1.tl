local common = require "common"

local function valid(line: common.LineData): boolean
    local _, n: any, number = line.pwd:gsub(line.letter, function() end)
    return (n >= line.min and n <= line.max)
end

print(common.count_valid(valid))
