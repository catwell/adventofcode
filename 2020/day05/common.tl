local util = require "util"

local M = {}

local function bin_lookup(left: integer, right: integer, stack: string): integer
    if stack == "" then
        assert(left == right)
        return left
    end
    local head, tail = stack:sub(1,1), stack:sub(2)
    if head == "F" or head == "L" then
        return bin_lookup(left, (left + right) // 2, tail)
    else
        return bin_lookup((left + right + 1) // 2, right, tail)
    end
end

local function seat_id(spec: string) : integer
    return 8 * bin_lookup(0, 127, spec:sub(1,7)) + bin_lookup(0, 7, spec:sub(8))
end

function M.seat_ids() : {integer}
    local lines = util.read_lines("input.txt")
    return util.map(lines, seat_id)
end

return M
