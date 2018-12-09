local util = require "util"

local function parse_lines(lines)
    local r = {}
    local ptrn = "Step (%w) must be finished before step (%w) can begin."
    for _, l in ipairs(lines) do
        local s, d = string.match(l, ptrn)
        if not r[s] then r[s] = {} end
        if not r[d] then r[d] = {} end
        r[d][s] = true
    end
    return r
end

local deps = parse_lines(util.read_lines(arg[1]))

local function next_step()
    local r = {}
    for s, l in pairs(deps) do
        if not next(l) then
            table.insert(r, s)
        end
    end
    table.sort(r)
    return r[1]
end

local function remove_step(s)
    deps[s] = nil
    for _, l in pairs(deps) do
        l[s] = nil
    end
end

local r = {}
while true do
    local s = next_step()
    if not s then
        print(table.concat(r))
        return
    end
    table.insert(r, s)
    remove_step(s)
end
