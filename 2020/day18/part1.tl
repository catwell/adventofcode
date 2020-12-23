local common = require "common"

local function flat_eval(s: string): number
    local pos, _, op = s:find("([+*])[^+*]+$")
    if not pos then return tonumber(s) end
    local left = flat_eval(s:sub(1, pos - 1))
    local right = tonumber(s:sub(pos + 1))
    if op == "+" then
        return left + right
    else
        return left * right
    end
end

print(common.compute(flat_eval))
