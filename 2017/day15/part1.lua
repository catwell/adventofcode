local ga_init, gb_init = tonumber(arg[1]), tonumber(arg[2])
local iterations = tonumber(arg[3])
local ga_factor, gb_factor = 16807, 48271
local ring_v = 2147483647

local function match(a, b)
    return (a & 0xffff) == (b & 0xffff)
end

local function generator(init, factor)
    local v = init
    return function()
        v = (v * factor) % ring_v
        return v
    end
end

local A = generator(ga_init, ga_factor)
local B = generator(gb_init, gb_factor)

local c = 0

for _ = 1, iterations do
    local a, b = A(), B()
    if match(a, b) then
        c = c + 1
    end
end

print(c)
