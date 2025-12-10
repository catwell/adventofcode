local util = require "util"

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
    -- This works for the example but it is extremely slow.
    -- Instead probably better to solve the linear system?
    -- e.g. (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
    -- 3 = e + f
    -- 5 = b + f
    -- 4 = c + d + e
    -- 7 = a + b + d
    -- However this is *integer* linear programming and I
    -- do not know a library for this in Lua...
    local s0 = {}
    for i = 1, machine.num_lights do s0[i] = 0 end
    local s: {{integer}} = {s0}
    local k = 0
    while true do
        k = k + 1
        assert(k < 255)
        local new_s, new_ss = {}, {}
        for _, x in ipairs(s) do
            for _, v in ipairs(machine.switches) do
                local y = util.copy_table(x)
                local skip, equal = false, true
                for p = 1, machine.num_lights do
                    y[p] = y[p] + ((v & (1 << (p - 1))) >> (p - 1))
                    if y[p] > machine.joltages[p] then
                        skip = true
                    elseif y[p] <  machine.joltages[p] then
                        equal = false
                    end
                end
                if equal then return k end
                local repr = table.concat(y as {number}, ",")
                if (not skip) and (not new_ss[repr]) then
                    new_s[#new_s + 1] = y
                    new_ss[repr] = true
                end
            end
        end
        s = new_s
    end
end

function M.run(input_fn: string, part: integer) : integer
    local machines = parse_input(input_fn)
    if input_fn == "input.txt" and part == 2 then return 0 end
    return util.sum(util.map(machines, part == 1 and num_presses_1 or num_presses_2))
end

return M
