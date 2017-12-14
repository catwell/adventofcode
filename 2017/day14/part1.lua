local knothash = require "knothash"
local base2base = require "base2base"
local fmt = string.format

local to_bin = base2base.converter(base2base.ALPHABET_B256, "01")

local key = arg[1]
local c = 0
for i = 0, 127 do
    local rowkey = fmt("%s-%d", key, i)
    local h = knothash.hash(rowkey)
    local b = to_bin(h)
    b:gsub("1", function() c = c + 1 end)
end

print(c)
