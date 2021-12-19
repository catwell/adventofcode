local util = require "util"

local type Point = {integer, integer}

local record Octopus
   level: integer
   flashed: boolean
end

local record State
   map: {{Octopus}}
   total_flashes: integer
   step_flashes: integer
   step: integer
   tick: function(State)
   process_flashes: function(State): boolean
end

local function parse_line(s: string) : {Octopus}
    local t, p = {}, 1
    local function f(x: string)
        t[p] = {
            level = math.tointeger(x),
            flashed = false,
        }
        p = p + 1
    end
    s:gsub("[0-9]", f)
    return t
end

local function parse_input() : {{Octopus}}
    local lines = util.read_lines("input.txt")
    return util.map(lines, parse_line)
end

local function neighbors(p: Point): {Point}
    local x, y = p[1], p[2]
    return {
        {x - 1, y - 1},
        {x - 1, y},
        {x - 1, y + 1},
        {x, y - 1},
        {x, y + 1},
        {x + 1, y - 1},
        {x + 1, y},
        {x + 1, y + 1},
    }
end

local function state_process_flashes(self: State): boolean
    local has_flashed = false
    for row = 1, 10 do
        for col = 1, 10 do
            local o = self.map[row][col]
            if not o.flashed and o.level > 9 then
                for _, p in ipairs(neighbors({row, col})) do
                    local o2 = self.map[p[1]] and self.map[p[1]][p[2]]
                    if o2 and (not o2.flashed) then
                        o2.level = o2.level + 1
                    end
                end
                o.flashed = true
                self.total_flashes = self.total_flashes + 1
                self.step_flashes = self.step_flashes + 1
                has_flashed = true
            end
        end
    end
    return has_flashed
end

local function state_tick(self: State)
    self.step = self.step + 1
    self.step_flashes = 0
    for row = 1, 10 do
        for col = 1, 10 do
            local o = self.map[row][col]
            o.level = o.level + 1
        end
    end
    while self:process_flashes() do end
    for row = 1, 10 do
        for col = 1, 10 do
            local o = self.map[row][col]
            if o.flashed then o.level = 0 end
            o.flashed = false
        end
    end
end

local function part1(state: State): integer
    for _ = 1, 100 do state:tick() end
    return state.total_flashes
end

local function part2(state: State): integer
    while true do
        state:tick()
        if state.step_flashes == 100 then
            return state.step
        end
    end
end

local M = {}

function M.run(part: integer) : integer
    local map = parse_input()
    local state = {
        map = map,
        step = 0,
        total_flashes = 0,
        step_flashes = 0,
        tick = state_tick,
        process_flashes = state_process_flashes,
    }
    if part == 1 then
        return part1(state)
    else
        return part2(state)
    end
end

return M
