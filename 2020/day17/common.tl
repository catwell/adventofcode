local util = require "util"

local record Cube
    active: boolean
end

local type Grid = {{{{Cube}}}}

local record State
    grid: Grid
    four_d: boolean
    xmin: integer
    xmax: integer
    ymin: integer
    ymax: integer
    zmin: integer
    zmax: integer
    wmin: integer
    wmax: integer
    three_d_space: function(State, integer): {{{Cube}}}
    cube_at: function(State, integer, integer, integer, integer): Cube
    neighbors: function(State, integer, integer, integer, integer): {Cube}
    step: function(State): State
    count_active: function(State): integer
end

local M = {}

local new_state : function(boolean) : State

local function state_three_d_space(self: State, w: integer): {{{Cube}}}
    if not self.four_d then return self.grid[0] end
    if not self.grid[w] then self.grid[w] = {} end
    if w < self.wmin then self.wmin = w end
    if w > self.wmax then self.wmax = w end
    return self.grid[w]
end

local function state_cube_at(
    self: State, x: integer, y: integer, z: integer, w: integer
): Cube
    local space = self:three_d_space(w)
    if not space[z] then space[z] = {} end
    if not space[z][y] then space[z][y] = {} end
    if not space[z][y][x] then space[z][y][x] = {active = false} end
    if x < self.xmin then self.xmin = x end
    if x > self.xmax then self.xmax = x end
    if y < self.ymin then self.ymin = y end
    if y > self.ymax then self.ymax = y end
    if z < self.zmin then self.zmin = z end
    if z > self.zmax then self.zmax = z end
    return space[z][y][x]
end

local function state_neighbors(
    self: State, x: integer, y: integer, z: integer, w: integer
): {Cube}
    local r = {}
    local _dw = self.four_d and 1 or 0
    for dw = -_dw, _dw do
        for dz = -1, 1 do
            for dy = -1, 1 do
                for dx = -1, 1 do
                    if dx ~= 0 or dy ~= 0 or dz ~= 0 or dw ~= 0 then
                        r[#r+1] = self:cube_at(x + dx, y + dy, z + dz, w + dw)
                    end
                end
            end
        end
    end
    return r
end

local function _is_active(cube: Cube): boolean
    return cube.active
end

local function state_step(self: State): State
    local r = new_state(self.four_d)
    local _dw = self.four_d and 1 or 0
    local xmin, xmax = self.xmin - 1, self.xmax + 1
    local ymin, ymax = self.ymin - 1, self.ymax + 1
    local zmin, zmax = self.zmin - 1, self.zmax + 1
    local wmin, wmax = self.wmin - _dw, self.wmax + _dw
    for w = wmin, wmax do
        for z = zmin, zmax do
            for y = ymin, ymax do
                for x = xmin, xmax do
                    local n = util.count(self:neighbors(x, y, z, w), _is_active)
                    if self:cube_at(x, y, z, w).active then
                        r:cube_at(x, y, z, w).active = (n == 2 or n == 3)
                    else
                        r:cube_at(x, y, z, w).active = (n == 3)
                    end
                end
            end
        end
    end
    return r
end

local function state_count_active(self: State): integer
    local r = 0
    for w = self.wmin, self.wmax do
        for z = self.zmin, self.zmax do
            for y = self.ymin, self.ymax do
                for x = self.xmin, self.xmax do
                    if self:cube_at(x, y, z, w).active then
                        r = r + 1
                        end
                end
            end
        end
    end
    return r
end

local state_mt = {__index = {
    three_d_space = state_three_d_space,
    cube_at = state_cube_at,
    neighbors = state_neighbors,
    step = state_step,
    count_active = state_count_active,
}}


new_state = function(four_d: boolean): State
    return setmetatable({
        grid = {[0] = {}},
        four_d = four_d,
        xmin = 0,
        xmax = 0,
        ymin = 0,
        ymax = 0,
        zmin = 0,
        zmax = 0,
        wmin = 0,
        wmax = 0,
    }, state_mt)
end

function M.parse_input(four_d: boolean) : State
    local st = new_state(four_d)
    local lines = util.read_lines("input.txt")
    for y, line in ipairs(lines) do
        for x = 1, #line do
            if line:sub(x, x) == "#" then
                st:cube_at(x, y, 0, 0).active = true
            end
        end
    end
    return st
end

return M
