local M = {}

local function parse_line(l)
    local x1, y1, x2, y2
    x1, y1, y2 = l:match("x=(%d+), y=(%d+)..(%d+)")
    if x1 then
        x2 = x1
    else
        y1, x1, x2 = l:match("y=(%d+), x=(%d+)..(%d+)")
        assert(y1)
        y2 = y1
    end
    local r = {x1 = x1, x2 = x2, y1 = y1, y2 = y2}
    for k, v in pairs(r) do r[k] = tonumber(v) end
    return r
end

function M.parse_input(s_lines)
    local lines = {}
    local r = {
        xmin = math.huge, xmax = -math.huge,
        ymin = math.huge, ymax = -math.huge,
        map = {}, flowing_water = {},
        water_count = 1, still_count = 0,
    }
    for i, s in ipairs(s_lines) do
        local l = parse_line(s)
        lines[i] = l
        if l.x1 < r.xmin then r.xmin = l.x1 end
        if l.x2 > r.xmax then r.xmax = l.x2 end
        if l.y1 < r.ymin then r.ymin = l.y1 end
        if l.y2 > r.ymax then r.ymax = l.y2 end
    end
    for y = r.ymin, r.ymax do r.map[y] = {} end
    for _, l in ipairs(lines) do
        for y = l.y1, l.y2 do
            for x = l.x1, l.x2 do
                r.map[y][x] = "#"
            end
        end
    end
    r.flowing_water[1] = {x = 500, y = r.ymin}
    r.map[r.ymin][500] = "|"
    return r
end

local function can_flow_through(s)
    return (not s) or (s == "|")
end

local function tick(state)
    local new_flowing = {}
    for _, c in ipairs(state.flowing_water) do
        local x, y = c.x, c.y
        while y < state.ymax do
            if not can_flow_through(state.map[y + 1][x]) then
                local xl, xr = x, x
                while true do
                    if (
                        state.map[y][xl - 1] == "#" or
                        can_flow_through(state.map[y + 1][xl])
                    ) then break end
                    xl = xl - 1
                end
                while true do
                    if (
                        state.map[y][xr + 1] == "#" or
                        can_flow_through(state.map[y + 1][xr])
                    ) then break end
                    xr = xr + 1
                end
                if (
                    can_flow_through(state.map[y + 1][xl]) or
                    can_flow_through(state.map[y + 1][xr])
                ) then
                    for x2 = xl, xr do
                        -- It is important to count this way to deal with
                        -- the case where two streams reach the same platform
                        -- simultaneously.
                        if not state.map[y][x2] then
                            state.water_count = state.water_count + 1
                            state.map[y][x2] = "|"
                        end
                    end
                    if can_flow_through(state.map[y + 1][xl]) then
                        table.insert(new_flowing, {x = xl, y = y})
                    end
                    if can_flow_through(state.map[y + 1][xr]) then
                        table.insert(new_flowing, {x = xr, y = y})
                    end
                else
                    for x2 = xl, xr do
                        if not state.map[y][x2] then
                            state.water_count = state.water_count + 1
                        end
                        if state.map[y][x2] ~= "~" then
                            state.still_count = state.still_count + 1
                            state.map[y][x2] = "~"
                        end
                    end
                    if y > state.ymin then
                        table.insert(new_flowing, {x = x, y = y - 1})
                    end
                end
                break
            elseif not state.map[y + 1][x] then
                state.water_count = state.water_count + 1
                state.map[y + 1][x] = "|"
                y = y + 1
            else
                assert(state.map[y + 1][x] == "|")
                break
            end
        end
    end
    state.flowing_water = new_flowing
end

-- for debug purpose

function M.repr(state)
    local r = {}
    for y = state.ymin, state.ymax do
        local l = {}
        for x = state.xmin, state.xmax do
            table.insert(l, state.map[y][x] or " ")
        end
        table.insert(r, table.concat(l))
    end
    return table.concat(r, "\n")
end

function M.ppm_repr(state)
    local r = {}
    table.insert(r, "P3")
    table.insert(r, string.format(
        "%d %d 1",
        state.xmax - state.xmin + 1,
        state.ymax - state.ymin + 1
    ))
    local t = {
        [" "] = "1 1 1",
        ["#"] = "0 0 0",
        ["~"] = "0 0 1",
        ["|"] =  "1 0 0",
    }
    for y = state.ymin, state.ymax do
        for x = state.xmin, state.xmax do
            table.insert(r, t[state.map[y][x] or " "])
        end
    end
    return table.concat(r, "\n")
end

function M.run(state)
    while #state.flowing_water > 0 do
        tick(state)
    end
end

return M
