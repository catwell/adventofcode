local util = require "util"

local M = {}

function M.parse_input() : {number}
    return util.map(util.read_lines("input.txt"), tonumber as function(string): number)
end

function M.find_invalid(input: {number}): number
    local window_size = 25
    for p = window_size + 1, #input do
        local l = {table.unpack(input, p - window_size, p - 1)}
        local s = util.to_set(l)
        local v = input[p]
        local found = false
        for _, n in ipairs(l) do
            if s[v - n] then
                found = true
                break
            end
        end
        if not found then
            return v
        end
    end
end

return M
