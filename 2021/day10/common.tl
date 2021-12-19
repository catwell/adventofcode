local util = require "util"

local record Map1Row
    op: string
    score: integer
end

local MAP1 : {string: Map1Row} = {
    [")"] = { op = "(", score = 3 },
    ["]"] = { op = "[", score = 57 },
    ["}"] = { op = "{", score = 1197 },
    [">"] = { op = "<", score = 25137 },
}

local function check_corrupt(line: string): integer, {string}
    local s : {string} = {}
    for p = 1, #line do
        local c = line:sub(p, p)
        if MAP1[c] then
            if s[#s] == MAP1[c].op then
                s[#s] = nil
            else
                return MAP1[c].score, s
            end
        else
            s[#s+1] = c
        end
    end
    return 0, s
end

local function part1(lines: {string}) : integer
    local r = 0
    for _, line in ipairs(lines) do
        r = r + check_corrupt(line)
    end
    return r
end

local MAP2 : {string: integer} = {
    ["("] = 1,
    ["["] = 2,
    ["{"] = 3,
    ["<"] = 4,
}

local function score2(s: {string}): integer
    local r = 0
    for i = #s, 1, -1 do
        r = r * 5 + MAP2[s[i]]
    end
    return r
end

local function part2(lines: {string}) : integer
    local r = {}
    for _, line in ipairs(lines) do
        local score, s = check_corrupt(line)
        if score == 0 then
            r[#r+1] = score2(s)
        end
    end
    table.sort(r)
    return r[#r//2+1]
end

local M = {}

function M.run(part: integer) : integer
    local lines = util.read_lines("input.txt")
    if part == 1 then
        return part1(lines)
    else
        return part2(lines)
    end
end

return M
