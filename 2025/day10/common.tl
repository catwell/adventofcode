local util = require "util"
local ilp_solver = require "ilp_solver"

local M = {}

local record Machine
    target: integer
    num_lights: integer
    switches: {integer}
    joltages: {integer}
end

local function parse_input(input_fn: string): {Machine}
    local lines = util.read_lines(input_fn)
    local r: {Machine} = {}
    for i, l in ipairs(lines) do
        local target_s, switches_s, voltages_s = l:match("%[(.-)%](.-)%{(.-)%}")
        local switches = {}
        for switch_s in switches_s:gmatch("%((.-)%)") do
            local switch_i = 0
            for _, v in ipairs(util.parse_integers(switch_s)) do
                switch_i = switch_i + (1 << v)
            end
            switches[#switches + 1] = switch_i
        end
        local num_lights = #target_s
        local target_i = 0
        for v = 1, num_lights do
            if target_s:sub(v, v) == "#" then
                target_i = target_i + (1 << (v - 1))
            end
        end
        r[i] = {
            target = target_i,
            num_lights = num_lights,
            switches = switches,
            joltages = util.parse_integers(voltages_s),
        }
    end
    return r
end

local function num_presses_1(machine: Machine): integer
    local s: {integer:boolean} = {[0] = true}
    local k = 0
    while true do
        if s[machine.target] then return k end
        k = k + 1
        local new_s = {}
        for x in pairs(s) do
            for _, v in ipairs(machine.switches) do
                local y = x ~ v
                if y == machine.target then return k end
                new_s[y] = true
            end
        end
        s = new_s
    end
end

local function num_presses_2(machine: Machine): integer
    local a = {}
    for i = 1, machine.num_lights do
        local t = {}
        for j, v in ipairs(machine.switches) do
            t[j] = ((v & (1 << (i - 1))) >> (i - 1))
        end
        a[i] = t
    end
    local solver = ilp_solver.new(a, machine.joltages)
    solver.search_limit = util.max(machine.joltages)
    local _, cost = solver:solve()
    return math.tointeger(cost)
end

function M.run(input_fn: string, part: integer) : integer
    local machines = parse_input(input_fn)
    return util.sum(util.map(machines, part == 1 and num_presses_1 or num_presses_2))
end

return M
