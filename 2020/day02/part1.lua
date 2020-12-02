local common = require "common"

local function valid(line)
    local _, n = line.pwd:gsub(line.letter, function() end)
    return (n >= line.min and n <= line.max)
end

print(common.count_valid(valid))
