-- I ended up using a prioritized octree search here.
-- There are probably better ways to do this, but it works with the input.

local util = require "util"
local common = require "common"

local function within_segment(n, s)
    return n >= s.min and n <= s.max
end

local function point_in_box(box, p)
    return (
        within_segment(p.x, box.x) and
        within_segment(p.y, box.y) and
        within_segment(p.z, box.z)
    )
end

local function box_vertices(box)
    return {
        {x = box.x.min, y = box.y.min, z = box.z.min},
        {x = box.x.min, y = box.y.min, z = box.z.max},
        {x = box.x.min, y = box.y.max, z = box.z.min},
        {x = box.x.min, y = box.y.max, z = box.z.max},
        {x = box.x.max, y = box.y.min, z = box.z.min},
        {x = box.x.max, y = box.y.min, z = box.z.max},
        {x = box.x.max, y = box.y.max, z = box.z.min},
        {x = box.x.max, y = box.y.max, z = box.z.max},
    }
end

local function bot_range_vertices(bot)
    return {
        {x = bot.x - bot.r, y = bot.y, z = bot.z},
        {x = bot.x + bot.r, y = bot.y, z = bot.z},
        {x = bot.x, y = bot.y - bot.r, z = bot.z},
        {x = bot.x, y = bot.y + bot.r, z = bot.z},
        {x = bot.x, y = bot.y, z = bot.z - bot.r},
        {x = bot.x, y = bot.y, z = bot.z + bot.r},
    }
end

local function box_in_range(bot, box)
    -- A cube is in range of a bot if either:
    -- - one of the vertices of the box is in range of the bot, or
    -- - one of the vertices of the range octaedron in is the box.
    -- Experimentally, checking in this order is much faster.
    for _, v in ipairs(box_vertices(box)) do
        if common.in_range(bot, v) then return true end
    end
    for _, v in ipairs(bot_range_vertices(bot)) do
        if point_in_box(box, v) then return true end
    end
    return false
end

local function segment_len(s)
    return s.max - s.min + 1
end

local function segment_mid(s)
    return (s.min + s.max) // 2
end

local function box_volume(box)
    return segment_len(box.x) * segment_len(box.y) * segment_len(box.z)
end

local function box_bots_count(box)
    return #box.bots
end

local function box_distance_to_origin(box)
    return (
        math.abs(segment_mid(box.x)) +
        math.abs(segment_mid(box.y)) +
        math.abs(segment_mid(box.z))
    )
end

local function box_cmp(b1, b2)
    if box_bots_count(b1) < box_bots_count(b2) then
        return true
    elseif box_bots_count(b2) < box_bots_count(b1) then
        return false
    end
    if box_volume(b1) > box_volume(b2) then
        return true
    elseif box_volume(b2) > box_volume(b1) then
        return false
    end
    return (box_distance_to_origin(b1) > box_distance_to_origin(b2))
end

local function split_box(box)
    -- NOTE: some boxes may be invalid, filter them out in caller!
    local sx1 = {min = box.x.min, max = segment_mid(box.x)}
    local sx2 = {min = sx1.max + 1, max = box.x.max}
    local sy1 = {min = box.y.min, max = segment_mid(box.y)}
    local sy2 = {min = sy1.max + 1, max = box.y.max}
    local sz1 = {min = box.z.min, max = segment_mid(box.z)}
    local sz2 = {min = sz1.max + 1, max = box.z.max}
    return {
        { x = sx1, y = sy1, z = sz1 },
        { x = sx1, y = sy1, z = sz2 },
        { x = sx1, y = sy2, z = sz1 },
        { x = sx1, y = sy2, z = sz2 },
        { x = sx2, y = sy1, z = sz1 },
        { x = sx2, y = sy1, z = sz2 },
        { x = sx2, y = sy2, z = sz1 },
        { x = sx2, y = sy2, z = sz2 },
    }
end

local function is_valid_seg(s)
    return s.min <= s.max
end

local function is_valid_box(box)
    return (
        is_valid_seg(box.x) and
        is_valid_seg(box.y) and
        is_valid_seg(box.z)
    )
end

local function invalid_segment()
    return { min = math.huge, max = -math.huge }
end

local function bots_in_range_of_box(bots, box)
    local r = {}
    for _, bot in ipairs(bots) do
        if box_in_range(bot, box) then
            table.insert(r, bot)
        end
    end
    return r
end

local function initial_box(bots)
    local r = {
        x = invalid_segment(),
        y = invalid_segment(),
        z = invalid_segment(),
    }
    for _, b in ipairs(bots) do
        local xmin, xmax = b.x - b.r, b.x + b.r
        local ymin, ymax = b.y - b.r, b.y + b.r
        local zmin, zmax = b.z - b.r, b.z + b.r
        if xmin < r.x.min then r.x.min = xmin end
        if xmax > r.x.max then r.x.max = xmax end
        if ymin < r.y.min then r.y.min = ymin end
        if ymax > r.y.max then r.y.max = ymax end
        if zmin < r.z.min then r.z.min = zmin end
        if zmax > r.z.max then r.z.max = zmax end
    end
    r.bots = bots_in_range_of_box(bots, r)
    return r
end

local bots = common.parse_lines(util.read_lines(arg[1]))
local boxes = {initial_box(bots)}

local function ordered_insert(t, v, cmp)
    local p1, p2 = 1, #t
    while p1 <= p2 do
        local m = (p1 + p2) // 2
        if cmp(t[m], v) then
            p1 = m + 1
        else
            p2 = m - 1
        end
    end
    table.insert(t, p1, v)
end

while true do
    local box = table.remove(boxes)
    if box_volume(box) == 1 then
        print(box_distance_to_origin(box))
        return
    end
    for _, b in ipairs(split_box(box)) do
        if is_valid_box(b) then
            -- Only check the bots that were in range of the parent box.
            b.bots = bots_in_range_of_box(box.bots, b)
            ordered_insert(boxes, b, box_cmp)
        end
    end
end
