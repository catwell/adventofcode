local util = require "util"

local M = {}

local function parse_input(input_fn: string): {string: {string}}
    local lines = util.read_lines(input_fn)
    local r = {}
    for _, l in ipairs(lines) do
        local t = util.split(l, ": ")
        r[t[1]] = util.split(t[2], " ")
    end
    return r
end

local function count_paths_1(wires: {string: {string}}): integer
    local cur_paths: {string: integer} = {you = 1}
    local total_you = 0
    while next(cur_paths) do
        local next_paths: {string: integer} = {}
        for k, v in pairs(cur_paths) do
            if wires[k] then
                for _, k2 in ipairs(wires[k]) do
                    next_paths[k2] = (next_paths[k2] or 0) + v
                end
            else
                assert(k == "out")
                total_you = total_you + v
            end
        end
        cur_paths = next_paths
    end
    return total_you
end

local function count_paths_2(wires: {string: {string}}): integer
    local cur_paths: {string: integer} = {svr = 1}
    local cur_fft: {string: integer} = {}
    local cur_dac: {string: integer} = {}
    local cur_both: {string: integer} = {}
    local over = false
    local total_you = 0
    while not over do
        local next_paths: {string: integer} = {}
        local next_fft: {string: integer} = {}
        local next_dac: {string: integer} = {}
        local next_both: {string: integer} = {}
        over = true
        for k, v in pairs(cur_paths) do
            if wires[k] then
                over = false
                for _, k2 in ipairs(wires[k]) do
                    local next_t = next_paths
                    if k2 == "fft" then
                        next_t = next_fft
                    elseif k2 == "dac" then
                        next_t = next_dac
                    end
                    next_t[k2] = (next_t[k2] or 0) + v
                end
            else
                assert(k == "out")
            end
        end
        for k, v in pairs(cur_fft) do
            if wires[k] then
                over = false
                for _, k2 in ipairs(wires[k]) do
                    local next_t = next_fft
                    if  k2 == "dac" then
                        next_t = next_both
                    end
                    next_t[k2] = (next_t[k2] or 0) + v
                end
            else
                assert(k == "out")
            end
        end
        for k, v in pairs(cur_dac) do
            if wires[k] then
                over = false
                for _, k2 in ipairs(wires[k]) do
                    local next_t = next_dac
                    if  k2 == "fft" then
                        next_t = next_both
                    end
                    next_t[k2] = (next_t[k2] or 0) + v
                end
            else
                assert(k == "out")
                -- total_you = total_you + v
            end
        end
        for k, v in pairs(cur_both) do
            if wires[k] then
                for _, k2 in ipairs(wires[k]) do
                    next_both[k2] = (next_both[k2] or 0) + v
                end
            else
                assert(k == "out")
                total_you = total_you + v
            end
        end
        cur_paths = next_paths
        cur_fft = next_fft
        cur_dac = next_dac
        cur_both = next_both
    end
    return total_you
end

function M.run(input_fn: string, part: integer) : integer
    local wires = parse_input(input_fn)
    if part == 1 then return count_paths_1(wires) end
    return count_paths_2(wires)
end

return M
