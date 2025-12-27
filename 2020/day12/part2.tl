local common = require "common"

local record Waypoint
    x: integer
    y: integer
    move: function(Waypoint, common.Move)
end

local record Ship
    x: integer
    y: integer
    wp: Waypoint
    move: function(Ship, common.Move)
end

local function waypoint_move(self: Waypoint, move: common.Move)
    if move.ins == "N" then
         self.y = self.y + move.arg
    elseif move.ins == "S" then
        self.y = self.y - move.arg
    elseif move.ins == "E" then
        self.x = self.x + move.arg
    elseif move.ins == "W" then
        self.x = self.x - move.arg
    elseif move.ins == "R" then
        for _ = 90, move.arg, 90 do
            self.x, self.y = self.y, -self.x
        end
    elseif move.ins == "L" then
        for _ = 90, move.arg, 90 do
            self.x, self.y = -self.y, self.x
        end
    end
end

local waypoint_mt = {__index = {move = waypoint_move}}

local function new_waypoint(): Waypoint
    return setmetatable({x = 10, y = 1}, waypoint_mt)
end

local function ship_move(self: Ship, move: common.Move)
    if move.ins == "F" then
        for _ = 1, move.arg do
            self.x = self.x + self.wp.x
            self.y = self.y + self.wp.y
        end
    else
        self.wp:move(move)
    end
end

local ship_mt = {__index = {move = ship_move}}

local function new_ship(): Ship
    return setmetatable({x = 0, y = 0, wp = new_waypoint()}, ship_mt)
end

local function main()
    local moves = common.parse_input()
    local ship = new_ship()
    for _, move in ipairs(moves) do
        ship:move(move)
    end
    print(math.abs(ship.x) + math.abs(ship.y))
end

main()
