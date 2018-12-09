local util = require "util"
local common = require "common"

local input = util.read_file(arg[1])

local t = common.to_byte_table(input)

local function filter(t, c)
    local r = {}
    for i = 1, #t do
        if t[i] ~= c and t[i] ~= common.opposite(c) then
            table.insert(r, t[i])
        end
    end
    return r
end

local min_len = math.huge
for c = string.byte("a"), string.byte("z") do
    local t2 = filter(t, c)
    local l = common.react(t2)
    min_len = math.min(min_len, l)
end

print(min_len)
