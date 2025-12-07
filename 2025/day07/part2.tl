local util = require "util"

local function zeroes(n: integer): {integer}
    local r = {}
    for i = 1, n do r[i] = 0 end
    return r
end

local function run(input_fn: string) : integer
    local lines = util.read_lines(input_fn)
    local n = #lines[1]
    for _, l in ipairs(lines) do
        assert(#l == n)
        assert(l:sub(n, n) == ".")
    end
    local pos = zeroes(n)
    for p = 1, n do
        if lines[1]:sub(p, p) == "S" then
            pos[p] = 1
        end
    end
    for row = 2, #lines do
        local l = lines[row]
        local next_pos = zeroes(n)
        for p = 1, n do
            if l:sub(p, p) == "^" then
                next_pos[p-1] = next_pos[p-1] + pos[p]
                next_pos[p+1] = next_pos[p+1] + pos[p]
            else
                next_pos[p] = next_pos[p] + pos[p]
            end
        end
        pos = next_pos
    end
    return util.sum(pos)
end

print("example: " .. tostring(run("example.txt")))
print("input: " .. tostring(run("input.txt")))
