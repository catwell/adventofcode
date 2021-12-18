local common = require "common"
local util = require "util"

local input = common.parse_input()

local function str_to_set(s: string): {string: boolean}
    local r = {}
    for i = 1, #s do
        r[s:sub(i, i)] = true
    end
    return r
end

local function full_set(): {string: boolean}
    return {
        a = true,
        b = true,
        c = true,
        d = true,
        e = true,
        f = true,
        g = true,
    }
end

local function init_pv(): {string: {string: boolean}}
    local ls = {'a', 'b', 'c', 'd', 'e', 'f', 'g'}
    local r = {}
    for _, v in ipairs(ls) do
        r[v] = full_set()
    end
    return r
end

local map : {string: integer} = {
    abcefg = 0,
    cf = 1,
    acdeg = 2,
    acdfg = 3,
    bcdf = 4,
    abdfg = 5,
    abdefg = 6,
    acf = 7,
    abcdefg = 8,
    abcdfg = 9,
}

local by_seg_count = {
    [2] = str_to_set('cf'),
    [4] = str_to_set('bcdf'),
    [3] = str_to_set('acf'),
    [7] = str_to_set('abcdefg'),
}

local always_5 = str_to_set('adg')
local always_6 = str_to_set('abfg')

local r = 0
for _, line in ipairs(input) do
    local pv = init_pv()
    for pos = 1, 10 do
        local s = line[pos]
        local n = #s
        local exact = by_seg_count[n]
        local direct = str_to_set(s)
        local complement = full_set()
        util.set_subtract(complement, direct)
        if exact then
            for v in pairs(direct) do
                util.set_intersect(pv[v], exact)
            end
            for v in pairs(complement) do
                util.set_subtract(pv[v], exact)
            end
        elseif n == 5 then
            for v in pairs(complement) do
                util.set_subtract(pv[v], always_5)
            end
        elseif n == 6 then
            for v in pairs(complement) do
                util.set_subtract(pv[v], always_6)
            end
        end
    end

    while true do
        local over = true
        local single = {}
        for _, v in pairs(pv) do
            if util.count_keys(v) > 1 then
                over = false
            else
                single[next(v)] = true
            end
        end
        if over then break end
        for _, v in pairs(pv) do
            if util.count_keys(v) > 1 then
                util.set_subtract(v, single)
            end
        end
    end

    local n, m = 0, 1
    for pos = 14, 11, -1 do
        local t : {string} = {}
        local function f(c: string)
            local v = next(pv[c])
            table.insert(t, v)
        end
        line[pos]:gsub('.', f)
        table.sort(t)
        n = n + map[table.concat(t)] * m
        m = m * 10
    end
    r = r + n
end

print(r)
