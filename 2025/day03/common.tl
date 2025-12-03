local util = require "util"

local M = {}

local function find_largest(t: {integer}, p_start: integer, p_end: integer): integer
    local p = p_start
    for i = p + 1, p_end do
        if t[i] > t[p] then p = i end
    end
    return p
end

local function get_voltage(digits: {integer}, ndigits: integer): integer
    local pos: {integer} = {[0] = 0}
    local s = 0
    for cur = 1, ndigits do
        local exp = ndigits - cur
        pos[cur] = find_largest(digits, pos[cur - 1] +  1, #digits - exp)
        s = s + math.tointeger(10^exp) * digits[pos[cur]]
    end
    return s
end

function M.run(input_fn: string, part: integer) : integer
    local lines = util.read_lines(input_fn)
    local ndigits = 2
    if part == 2 then  ndigits = 12 end
    local s = 0
    for _, line in ipairs(lines) do
        local digits = util.parse_digits(line)
        s = s + get_voltage(digits, ndigits)
    end
    return s
end

return M
