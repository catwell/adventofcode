local util = require "util"

local enum Instruction
    "N"
    "S"
    "E"
    "W"
    "L"
    "R"
    "F"
end

local enum Direction
    "N"
    "S"
    "E"
    "W"
end

local record Move
   ins: Instruction
   arg: integer
end

local M = {
    Instruction = Instruction,
    Direction = Direction,
    Move = Move,
}

local function parse_line(l: string) : Move
    local ins, argv = l:match("^([NSEWLRF])(%d+)$")
    return {
        ins = ins as Instruction,
        arg = math.tointeger(argv),
    }
end

function M.parse_input() : {Move}
    local lines = util.read_lines("input.txt")
    return util.map(lines, parse_line)
end

return M
