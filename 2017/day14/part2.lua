local knothash = require "knothash"
local base2base = require "base2base"
local fmt = string.format

local to_bin = base2base.converter(base2base.ALPHABET_B256, "01")

local MEM = setmetatable(
    {}, {
        __index = function(t, k)
            t[k] = {}
            return t[k]
        end
    }
)

local key = arg[1]
for i = 0, 127 do
    local rowkey = fmt("%s-%d", key, i)
    local h = knothash.hash(rowkey)
    local b = to_bin(h)
    local p = 128 - #b
    for j = 0, 127 do
        if j >= p and b:sub(j - p + 1, j - p + 1) == "1" then
            MEM[i][j] = true
        end
    end
end

local function find_component()
    for i = 0, 127 do
        for j = 0, 127 do
            if MEM[i][j] then
                return i, j
            end
        end
    end
end

local function destroy_component(x, y)
    if MEM[x][y] then
        MEM[x][y] = nil
        destroy_component(x - 1, y)
        destroy_component(x + 1, y)
        destroy_component(x, y - 1)
        destroy_component(x, y + 1)
    end
end

local c = 0
while true do
    local x, y = find_component()
    if not x then
        print(c)
        return
    end
    destroy_component(x, y)
    c = c + 1
end
