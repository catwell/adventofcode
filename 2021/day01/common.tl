local util = require "util"

local M = {}

local function parse_input() : {integer}
    return util.parse_integers(util.read_file("input.txt"))
end

function M.compute(window_size: integer) : integer
    local input = parse_input()
    local r = 0
    for i = window_size + 1, #input do
        if input[i] > input[i-window_size] then
            r = r + 1
        end
    end
    return r
end

return M
