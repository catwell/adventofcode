local MAP = setmetatable(
    {}, {
        __index = function(t, k)
            t[k] = {}
            return t[k]
        end
    }
)
local D, Y, X, Ymin, Ymax, Xmin, Xmax

local function load()
    for l in io.lines(arg[1]) do
        local t = {}
        local function f(x) table.insert(t, x) end
        l:gsub(".", f)
        table.insert(MAP, t)
    end
    D = "n"
    local n = #MAP[1]
    Xmin, Ymin, Xmax, Ymax = 1, 1, n, n
    X = (n + 1) // 2
    Y = X
end

local function cleanup()
    for y = 1, #MAP do
        for x = 1, #MAP[y] do
            if MAP[y][x] == "." then
                MAP[y][x] = nil
            else
                MAP[y][x] = true
            end
        end
    end
end

local function move()
    if D == "n" then
        Y = Y - 1
        if Y < Ymin then Ymin = Y end
    elseif D == "s" then
        Y = Y + 1
        if Y > Ymax then Ymax = Y end
    elseif D == "e" then
        X = X + 1
        if X > Xmax then Xmax = X end
    elseif D == "w" then
        X = X - 1
        if X < Xmin then Xmin = X end
    else error("?") end
end

local _d_right = { n = "e", e = "s", s = "w", w = "n" }
local _d_left = { n = "w", w = "s", s = "e", e = "n" }

local C = 0

local function burst()
    if MAP[Y][X] then
        D = _d_right[D]
        MAP[Y][X] = nil
    else
        D = _d_left[D]
        MAP[Y][X] = true
        C = C + 1
    end
    move()
end

local function print_grid()
    for y = Ymin, Ymax do
        local t = {}
        for x = Xmin, Xmax do
            if y == Y and x == X then
                table.insert(t, "o")
            else
                table.insert(t, MAP[y][x] and "#" or ".")
            end
        end
        print(table.concat(t))
    end
end

load()
cleanup()

for _ = 1, 10000 do
    burst()
end

local PRINT_GRID = false
if PRINT_GRID then print_grid() end

print(C)
