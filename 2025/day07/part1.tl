local util = require "util"

local function run(input_fn: string) : integer
    local lines = util.read_lines(input_fn)
    local n = #lines[1]
    for _, l in ipairs(lines) do
        assert(#l == n)
        assert(l:sub(n, n) == ".")
    end
    local pos: {integer:boolean} = {}
    for i = 1, n do
        if lines[1]:sub(i, i) == "S" then
            pos[i] = true
        end
    end
    local splits = 0
    for row = 2, #lines do
        local l = lines[row]
        local next_pos: {integer:boolean} = {}
        for p, _ in pairs(pos) do
            if l:sub(p, p) == "^" then
                next_pos[p-1] = true
                next_pos[p+1] = true
                splits = splits + 1
            else
                next_pos[p] = true
            end
        end
        pos = next_pos
    end
    return splits
end

print("example: " .. tostring(run("example.txt")))
print("input: " .. tostring(run("input.txt")))
