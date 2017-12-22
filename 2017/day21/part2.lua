local R2, R3 = {}, {}

local function parse_rule(l)
    local from, to = l:match("([%.%/%#]+) %=%> ([%.%/%#]+)")
    if not from then return end
    from = from:gsub("/", "")
    to = to:gsub("/", "")
    if #from == 4 then
        R2[from] = to
    else
        assert(#from == 9)
        R3[from] = to
    end
end

local function pattern_to_string(p)
    return table.concat(p)
end

local function string_to_pattern(s)
    local t = {}
    local function f(x) table.insert(t, x) end
    s:gsub(".", f)
    return t
end

local function rotate2(p)
    return {p[3], p[1], p[4], p[2]}
end

local function add_rot2(t, p)
    for _ = 1, 4 do
        p = rotate2(p)
        table.insert(t, p)
    end
end

local function poss2(p)
    local t = {}
    add_rot2(t, p)
    add_rot2(t, {p[2], p[1], p[4], p[3]}) -- horizontal flip
    add_rot2(t, {p[3], p[4], p[1], p[2]}) -- vertical flip
    return t
end

local function match2(p0)
    for _, p in ipairs(poss2(p0)) do
        local v = R2[pattern_to_string(p)]
        if v then return string_to_pattern(v) end
    end
    error(pattern_to_string(p0))
end

local function rotate3(p)
    return {p[7], p[4], p[1], p[8], p[5], p[2], p[9], p[6], p[3]}
end

local function add_rot3(t, p)
    for _ = 1, 4 do
        p = rotate3(p)
        table.insert(t, p)
    end
end

local function poss3(p)
    local t = {}
    add_rot3(t, p)
    add_rot3(t, {p[3], p[2], p[1], p[6], p[5], p[4], p[9], p[8], p[7]})
    add_rot3(t, {p[7], p[8], p[9], p[4], p[5], p[6], p[1], p[2], p[3]})
    return t
end

local function match3(p0)
    for _, p in ipairs(poss3(p0)) do
        local v = R3[pattern_to_string(p)]
        if v then return string_to_pattern(v) end
    end
    error(pattern_to_string(p0))
end

for l in io.lines(arg[1]) do
    parse_rule(l)
end

local grid_n, grid = 3, string_to_pattern(".#...####")

local function step()
    local new_grid, sz = {}
    if grid_n % 2 == 0 then
        sz = 2
    else
        assert(grid_n % 3 == 0)
        sz = 3
    end
    local c = grid_n // sz -- count per row / column
    local new_grid_n = grid_n + c
    for y = 0, c - 1 do
        for x = 0, c - 1 do
            local offset, v = y * sz * grid_n + x * sz + 1
            if sz == 2 then
                local p = {
                    grid[offset], grid[offset + 1],
                    grid[offset + grid_n], grid[offset + grid_n + 1],
                }
                v = match2(p)
            else
                local p = {
                    grid[offset], grid[offset + 1], grid[offset + 2],
                    grid[offset + grid_n], grid[offset + grid_n + 1], grid[offset + grid_n + 2],
                    grid[offset + 2 * grid_n], grid[offset + 2 * grid_n + 1], grid[offset + 2 * grid_n + 2],
                }
                v = match3(p)
            end
            offset = y * (sz + 1) * new_grid_n + x * (sz + 1) + 1
            for y2 = 0, sz do
                for x2 = 0, sz do
                    new_grid[offset + y2 * new_grid_n + x2] = v[(sz + 1) * y2 + x2 + 1]
                end
            end
        end
    end
    grid_n, grid = new_grid_n, new_grid
end

local function print_grid()
    for y = 0, grid_n - 1 do
        for x = 0, grid_n - 1 do
            io.write(grid[y * grid_n + x + 1])
        end
        io.write("\n")
    end
end

for _ = 1, 18 do
    step()
end


local c = 0
for i = 1, grid_n * grid_n do
    if grid[i] == "#" then
        c = c + 1
    else
        assert(grid[i] == ".")
    end
end

local PRINT_GRID = false
if PRINT_GRID then print_grid() end
print(c)
