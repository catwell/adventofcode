local util = require "util"

local type FlatEvaluator = function(string): number

local M = {}

local function evaluate(s: string, flat_eval: FlatEvaluator): number
    local t : {string} = {}
    local pos = 1
    while true do
        local start, stop = s:find("%b()", pos)
        if start then
            table.insert(t, s:sub(pos, start - 1))
            table.insert(t, tostring(evaluate(s:sub(start + 1, stop - 1), flat_eval)))
            pos = stop + 1
        else
            table.insert(t, s:sub(pos))
            break
        end
    end
    return flat_eval(table.concat(t))
end

function M.compute(flat_eval: FlatEvaluator): number
    local function f(s: string): number
        return evaluate(s, flat_eval)
    end
    return util.sum(util.map(util.read_lines("input.txt"), f))
end

return M
