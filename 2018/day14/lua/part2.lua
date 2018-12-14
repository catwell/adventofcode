local util = require "util"
local common = require "common"

local function figures(s)
    local t = {}
    local function f(x) table.insert(t, tonumber(x)) end
    s:gsub("%d", f)
    return t
end

local function run(s)
    local state = common.initial_state()
    local q = figures(s)
    local l = #q
    while state.n <= l do common.tick(state) end
    local function match(p)
        for i = 1, l do
            if state.s[p + i - 1] ~= q[i] then
                return false
            end
        end
        return true
    end
    while true do
        -- This is obviously not the fastest solution,
        -- but it works fast enough for me here. If the
        -- input was much larger it would make sense to
        -- use something like Aho–Corasick, but when in
        -- doubt use brute force :p
        if match(state.n - l - 1) then return state.n - l - 1 end
        if match(state.n - l) then return state.n - l end
        common.tick(state)
    end
end

print(run(util.read_file(arg[1])))
