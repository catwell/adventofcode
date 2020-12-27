local util = require "util"

local record Tile
    id: number
    borders: {string}
    data: {string}
end

local type Tiles = {number: Tile}

local type BorderIdMap = {string: {number}}

local M = {}

local function normalize_border(s: string): string
    local r = s:reverse()
    return r < s and r or s
end

local function get_borders(data: {string}): {string}
    -- returns borders from left to right by left rotation
    local t1, t2, n = {}, {}, #data
    for i = 1, n do
        t1[i] = data[i]:sub(1, 1)
        t2[i] = data[i]:sub(n, n)
    end
    return {
        data[1],
        table.concat(t2),
        data[n]:reverse(),
        table.concat(t1):reverse(),
    }
end

local function parse_tile(s: string): Tile
    local data = util.split(s, "\n")
    local l1 = table.remove(data, 1)
    local id = tonumber(l1:match("^Tile (%d+):$"))
    local borders = get_borders(data)
    return {
        id = id,
        borders = util.map(borders, normalize_border),
        data = data,
    }
end

local function rotate_left(data: {string}): {string}
    local r, t, n = {}, {}, #data
    for i = 1, n do
        local x = n - i + 1
        for j = 1, n do
            t[j] = data[j]:sub(x, x)
        end
        r[i] = table.concat(t)
    end
    return r
end

local function flip_horiz(data: {string}): {string}
    local r, n = {}, #data
    for i = 1, n do r[i] = data[i]:reverse() end
    return r
end

local function flip_vert(data: {string}): {string}
    return rotate_left(rotate_left(flip_horiz(data)))
end

function M.border_to_id(tiles: Tiles): BorderIdMap
    local r : {string: {number}} = {}
    for _, tile in pairs(tiles) do
        for _, border in ipairs(tile.borders) do
            if r[border] then
                table.insert(r[border], tile.id)
            else
                r[border] = {tile.id}
            end
        end
    end
    return r
end

function M.parse_input() : Tiles
    local r = {}
    for _, s in ipairs(util.split(util.read_file("input.txt"), "\n\n")) do
        local tile = parse_tile(s)
        r[tile.id] = tile
    end
    return r
end

local function count_external_borders(tile: Tile, b_map: BorderIdMap): number
    local r = 0
    for _, border in ipairs(tile.borders) do
        if #b_map[border] == 1 then r = r + 1 end
    end
    return r
end

function M.corner_ids(tiles: Tiles, b_map: BorderIdMap): {number}
    local r = {}
    for _, tile in pairs(tiles) do
        if count_external_borders(tile, b_map) == 2 then
            r[#r+1] = tile.id
        end
    end
    return r
end

local function orient_top_left_data(data: {string}, b_map: BorderIdMap) : {string}
    while #b_map[normalize_border(data[1])] == 1 do
        data = rotate_left(data)
    end
    while #b_map[normalize_border(data[1])] ~= 1 do
        data = rotate_left(data)
    end
    return rotate_left(data)
end

local function match_right_border(tile: Tile, tiles: Tiles, b_map: BorderIdMap): Tile
    local match = get_borders(tile.data)[2]
    local match_normalized = normalize_border(match)
    local match_ids = b_map[match_normalized]
    assert(#match_ids == 2)
    local r = tiles[match_ids[1]]
    if r.id == tile.id then
        r = tiles[match_ids[2]]
    end
    while normalize_border(get_borders(r.data)[4]) ~= match_normalized do
        r.data = rotate_left(r.data)
    end
    if get_borders(r.data)[4] == match then
        r.data = flip_vert(r.data)
    end
    return r
end

local function match_bottom_border(tile: Tile, tiles: Tiles, b_map: BorderIdMap): Tile
    local match = get_borders(tile.data)[3]
    local match_normalized = normalize_border(match)
    local match_ids = b_map[match_normalized]
    assert(#match_ids == 2)
    local r = tiles[match_ids[1]]
    if r.id == tile.id then
        r = tiles[match_ids[2]]
    end
    while normalize_border(get_borders(r.data)[1]) ~= match_normalized do
        r.data = rotate_left(r.data)
    end
    if get_borders(r.data)[1] == match then
        r.data = flip_horiz(r.data)
    end
    return r
end

local function get_grid_row(left: Tile, tiles: Tiles, b_map: BorderIdMap): {Tile}
    local r = {left}
    local corner_n = count_external_borders(left, b_map)
    while true do
        local next_tile = match_right_border(r[#r], tiles, b_map)
        r[#r + 1] = next_tile
        if count_external_borders(next_tile, b_map) == corner_n then break end
    end
    return r
end

function M.get_tile_grid(tiles: Tiles, b_map: BorderIdMap): {{Tile}}
    local ids = M.corner_ids(tiles, b_map)
    local top_left = tiles[ids[1]]

    top_left.data = orient_top_left_data(top_left.data, b_map)

    local r = {get_grid_row(top_left, tiles, b_map)}
    while true do
        local next_tile = match_bottom_border(r[#r][1], tiles, b_map)
        r[#r + 1] = get_grid_row(next_tile, tiles, b_map)
        if count_external_borders(next_tile, b_map) == 2 then break end
    end

    return r
end

function M.get_image(grid: {{Tile}}): {string}
    local r, n = {}, #grid[1][1].data
    for i = 1, #grid do
        for row = 2, n - 1 do
            local t : {string} = {}
            for j = 1, #grid[i] do
                local tile = grid[i][j]
                for col = 2, n - 1 do
                    t[#t + 1] = tile.data[row]:sub(col, col)
                end
            end
            r[#r + 1] = table.concat(t)
        end
    end
    return r
end

local function sea_monster_pattern(row: number, col: number): {{number}}
    return {
        {row + 1, col},
        {row + 2, col + 1},
        {row + 2, col + 4},
        {row + 1, col + 5},
        {row + 1, col + 6},
        {row + 2, col + 7},
        {row + 2, col + 10},
        {row + 1, col + 11},
        {row + 1, col + 12},
        {row + 2, col + 13},
        {row + 2, col + 16},
        {row + 1, col + 17},
        {row + 1, col + 18},
        {row + 1, col + 19},
        {row + 0, col + 18},
    }
end

local function has_sea_monster(image: {string}, row: number, col: number): boolean
    for _, p in ipairs(sea_monster_pattern(row, col)) do
        if image[p[1]]:sub(p[2], p[2]) ~= "#" then
            return false
        end
    end
    return true
end

local function mark_sea_monster(map: {{boolean}}, row: number, col: number)
    for _, p in ipairs(sea_monster_pattern(row, col)) do
        map[p[1]][p[2]] = true
    end
end

local function count_sea_monsters(image: {string}): number, number
    local c, m, n = 0, {{}, {}}, 0
    for row = 1, #image - 2 do
        m[row + 2] = {}
        for col = 1, #image[1] - 19 do
            if has_sea_monster(image, row, col) then
                c = c + 1
                mark_sea_monster(m, row, col)
            end
        end
    end
    if c then
        for row = 1, #image do
            for col = 1, #image[1] do
                if image[row]:sub(col, col) == "#" and (not m[row][col]) then
                    n = n + 1
                end
            end
        end
    end
    return c, n
end

function M.water_roughness(image: {string}): number
    for _ = 1, 2 do
        for _ = 1, 4 do
            local c, r = count_sea_monsters(image)
            if c > 0 then return r end
            image = rotate_left(image)
        end
        image = flip_horiz(image)
    end
    error("sea monsters not found")
end

return M
