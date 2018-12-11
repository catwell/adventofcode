local M = {}

-- Let's have fun and use an integral image, because why not.

function M.integral_image(serial)
    local G = {}

    do
        local r = {}
        for x = 0, 300 do r[x] = 0 end
        G[0] = r
    end

    local function power_level(x, y)
        local rack_id = x + 10
        local r = (y * rack_id + serial) * rack_id
        return r % 1000 // 100 - 5
    end

    for y = 1, 300 do
        local r = { [0] = 0 }
        for x = 1, 300 do
            r[x] = r[x - 1] + G[y - 1][x] - G[y - 1][x - 1] + power_level(x, y)
        end
        G[y] = r
    end

    return G
end

function M.square_power(G, x, y, n)
    x, y = x - 1, y - 1 -- evil trick :)
    return (
        G[y + n][x + n] + G[y][x] -
        G[y][x + n] - G[y + n][x]
    )
end

return M
