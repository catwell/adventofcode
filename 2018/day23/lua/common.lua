local M = {}

function M.parse_lines(lines)
    local ptrn = "pos=<([-%d]+),([-%d]+),([-%d]+)>, r=([%d]+)"
    local res = {}
    for i, l in ipairs(lines) do
        local x, y, z, r = string.match(l, ptrn)
        res[i] = {
            x = assert(tonumber(x)), y = assert(tonumber(y)),
            z = assert(tonumber(z)), r = assert(tonumber(r)),
        }
    end
    return res
end

local function distance(p1, p2)
    return math.abs(p1.x - p2.x) + math.abs(p1.y - p2.y) + math.abs(p1.z - p2.z)
end

-- a bot is a point with an extra radius
function M.in_range(bot, p)
    return distance(bot, p) <= bot.r
end

return M
