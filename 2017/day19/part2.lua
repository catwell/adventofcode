local MAP = setmetatable({}, {__index = function() return {} end})
local D, Y, X, N = "s", 1, 1, 0

for l in io.lines(arg[1]) do
    local t = {}
    local function f(x) table.insert(t, x) end
    l:gsub(".", f)
    table.insert(MAP, t)
end

local function cleanup()
    for y = 1, #MAP do
        for x = 1, #MAP[y] do
            if MAP[y][x] == " " then
                MAP[y][x] = nil
            end
        end
    end
end

local function find_start()
    while true do
        if MAP[1][X] then break end
        X = X + 1
    end
end

local function reached_end()
    print(N)
    os.exit()
end

local function try_horizontal()
    if MAP[Y][X + 1] then
        D = "e"
        X = X + 1
    elseif MAP[Y][X - 1] then
        D = "w"
        X = X - 1
    else
        reached_end()
    end
end

local function try_vertical()
    if MAP[Y - 1][X] then
        D = "n"
        Y = Y - 1
    elseif MAP[Y + 1][X] then
        D = "s"
        Y = Y + 1
    else
        reached_end()
    end
end

local function step()
    N = N + 1
    if D == "n" then
        if MAP[Y - 1][X] then
            Y = Y - 1
        else
            try_horizontal()
        end
    elseif D == "s" then
        if MAP[Y + 1][X] then
            Y = Y + 1
        else
            try_horizontal()
        end
    elseif D == "e" then
        if MAP[Y][X + 1] then
            X = X + 1
        else
            try_vertical()
        end
    elseif D == "w" then
        if MAP[Y][X - 1] then
            X = X - 1
        else
            try_vertical()
        end
    end
end

cleanup()
find_start()
while true do step() end
