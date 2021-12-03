local util = require "util"

local enum Direction
    "forward"
    "up"
    "down"
end

local record Move
   direction: Direction
   units: integer
end

local M = {}

local function parse_line(line: string) : Move
    local ptrn = "(%w+) (%d+)"
    local direction, units = string.match(line, ptrn)
    assert(units)
    return {
        direction = direction,
        units = units,
    }
end

function M.parse_input() : {Move}
    return util.map(util.read_lines("input.txt"), parse_line)
end

return M
