local input = tonumber(arg[1])

local ring = 0

while true do
    local n = (2 * ring + 1) ^ 2
    if input <= n then break end
    ring = ring + 1
end

local p = (2 * ring + 1) ^ 2 - ring -- bottom center

local m = math.huge
for i = 1, 4 do
    local n = math.abs(p - input)
    if n < m then m = n end
    p = p - 2 * ring
end

print(ring + math.floor(m))
