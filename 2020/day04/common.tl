local util = require "util"

local type Passport = {string:string}
local type ValidChecker = function(Passport): boolean

local M = {Passport = Passport}

local function passport_data_from_input() : {string}
    local input = util.read_file("input.txt") .. "\n\n"
    local r = {}
    for v in input:gmatch("(.-)\n\n") do
        r[#r+1] = v
    end
    return r
end

local function parse_passport_data(data: string) : Passport
    local t = {}
    local function f(k: string, v: string)
        t[k] = v
    end
    data:gsub("([^%s:]+):([^%s:]+)", f)
    return t
end


function M.count_valid(valid: ValidChecker): number
    local passports = util.map(passport_data_from_input(), parse_passport_data)
    local validity = util.map(passports, valid)
    local c = 0
    for _, v in ipairs(validity) do
        if v then
            c = c + 1
        end
    end
    return c
end

return M

