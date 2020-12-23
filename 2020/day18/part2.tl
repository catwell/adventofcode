local common = require "common"

local function flat_eval_sum(s: string): number
    local pos = s:find("%+[^+]+$")
    if not pos then return tonumber(s) end
    local left = flat_eval_sum(s:sub(1, pos - 1))
    local right = tonumber(s:sub(pos + 1))
    return left + right
end

local function flat_eval_mul(s: string): number
    local pos = s:find("%*[^*]+$")
    if pos then
        return flat_eval_mul(s:sub(1, pos - 1)) * flat_eval_sum(s:sub(pos + 1))
    else
        return flat_eval_sum(s)
    end
end

print(common.compute(flat_eval_mul))
