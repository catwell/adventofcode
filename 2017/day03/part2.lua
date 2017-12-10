local input = tonumber(arg[1])

local function deft_0()
    return setmetatable({}, {__index = function() return 0 end})
end

local core = setmetatable(
    {}, {
        __index = function(t, k)
            t[k] = deft_0()
            return t[k]
        end
    }
)

local x, y = 0, 0
core[0][0] = 1

while true do
    if y <= 0 and -y >= math.abs(x) then
        x = x + 1
    elseif x > math.abs(y) then
        y = y + 1
    elseif y > 0 and (y == x or y > math.abs(x)) then
        x = x - 1
    else
        y = y - 1
    end
    local n = 0
    for i = -1, 1 do
        for j = -1, 1 do
            if i ~= 0 or j ~= 0 then
                n = n + core[x + i][y + j]
            end
        end
    end
    core[x][y] = n
    if n > input then
        print(n)
        break
    end
end
