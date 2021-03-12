local common = require "common"

local Instruction = common.Instruction
local Direction = common.Direction
local Move = common.Move

local record Ship
    x: integer
    y: integer
    dir: Direction
    move: function(Ship, Move)
end

local function ship_move(self: Ship, move: Move)
    if move.ins == "N" then
         self.y = self.y + move.arg
    elseif move.ins == "S" then
        self.y = self.y - move.arg
    elseif move.ins == "E" then
        self.x = self.x + move.arg
    elseif move.ins == "W" then
        self.x = self.x - move.arg
    elseif move.ins == "R" then
        local _dirs = {N = "E", E = "S", S = "W", W = "N"}
        for _ = 90, move.arg, 90 do
            self.dir = _dirs[self.dir]
        end
    elseif move.ins == "L" then
        local _dirs = {N = "W", E = "N", S = "E", W = "S"}
        for _ = 90, move.arg, 90 do
            self.dir = _dirs[self.dir]
        end
    else
        assert(move.ins == "F")
        self:move({
            ins = self.dir as Instruction,
            arg = move.arg,
        })
    end
end

local ship_mt = {__index = {move = ship_move}}

local function new_ship(): Ship
    return setmetatable({x = 0, y = 0, dir = "E"}, ship_mt)
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
