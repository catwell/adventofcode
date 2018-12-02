local util = require "util"

local lines = util.read_lines(arg[1])

local function process_line(l)
    local counts, c2, c3 = {}, 0, 0
    for c in l:gmatch("%w") do
        counts[c] = (counts[c] or 0) + 1
        if counts[c] == 2 then
            c2 = c2 + 1
        elseif counts[c] == 3 then
            c2 = c2 - 1
            c3 = c3 + 1
        elseif counts[c] == 4 then
            c3 = c3 - 1
        end
    end
    return (c2 > 0), (c3 > 0)
end

local c2, c3 = 0, 0
for _, l in ipairs(lines) do
    local has_2, has_3 = process_line(l)
    if has_2 then c2 = c2 + 1 end
    if has_3 then c3 = c3 + 1 end
end

print(c2 * c3)
