local common = require "common"
local util = require "util"

local function candidate_sequences(t: {integer}): {{integer}}
    local r, prev = {}, 1
    for p = 2, #t do
        local d = t[p] - t[p-1]
        if d == 3 then
            r[#r+1] = {table.unpack(t, prev, p-1)}
            prev = p
        end
    end
    return r
end

local function valid(candidate: {integer}): boolean
    for p = 2, #candidate do
        if candidate[p] - candidate[p-1] > 3 then
            return false
        end
    end
    return true
end

local function arrangements(sequence: {integer}, processed: {string:boolean}): integer
    if not processed then processed = {} end
    local r = 1
    for i = 2, #sequence - 1 do
        local candidate = {table.unpack(sequence, 1, i - 1)}
        for j = i + 1, #sequence do
            candidate[#candidate + 1] = sequence[j]
        end
        if valid(candidate) then
            local s = table.concat(util.map(candidate, tostring as function(integer): string), '-')
            if not processed[s] then
                processed[s] = true
                r = r + arrangements(candidate, processed)
            end
        end
    end
    return r
end

local seqs = candidate_sequences(common.initial_arrangement())
print(util.product(util.map(seqs, arrangements as function({integer}): integer)))
