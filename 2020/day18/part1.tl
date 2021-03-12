local common = require "common"

local function flat_eval(s: string): integer
    local pos, _, op = s:find("([+*])[^+*]+$")
    if not pos then return math.tointeger(s) end
    local left = flat_eval(s:sub(1, pos - 1))
    local right = math.tointeger(s:sub(pos + 1))
    if op == "+" then
        return left + right
    else
        return left * right
    end
end

print(common.compute(flat_eval))
