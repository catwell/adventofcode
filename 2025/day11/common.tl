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
    local cur_paths: {string: {string: integer}} = {
        none = {svr = 1},
        fft = {},
        dac = {},
        both = {},
    }
    local over = false
    local total_you = 0
    local next_k: {string: {string: string}} = {
        none = {
            fft = "fft",
            dac = "dac",
        },
        fft = { dac = "both"},
        dac = { fft = "both"},
        both = {},
    }
    while not over do
        local next_paths: {string: {string: integer}} = {
            none = {},
            fft = {},
            dac = {},
            both = {},
        }
        over = true
        for part, paths in pairs(cur_paths) do
            for k, v in pairs(paths) do
                if wires[k] then
                    over = false
                    for _, k2 in ipairs(wires[k]) do
                        local next_part = next_k[part][k2] or part
                        local next_t = next_paths[next_part]
                        next_t[k2] = (next_t[k2] or 0) + v
                    end
                else
                    assert(k == "out")
                    if part == "both" then
                        total_you = total_you + v
                    end
                end
            end
        end
        cur_paths = next_paths
    end
    return total_you
end

function M.run(input_fn: string, part: integer) : integer
    local wires = parse_input(input_fn)
    if part == 1 then return count_paths_1(wires) end
    return count_paths_2(wires)
end

return M
