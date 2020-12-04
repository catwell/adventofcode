local common = require "common"

local function four_digits_between(v: string, s: string, e: string): boolean
    return (
        v and v:match("^%d%d%d%d$") and
        v >= s and v <= e
    )
end

local function valid_hgt(v: string): boolean
    if not v then return false end
    local ns, unit = v:match("^(%d+)(%l+)$")
    local n = tonumber(ns)
    if unit == "cm" then
        return (n and n >= 150 and n <= 193)
    elseif unit == "in" then
        return (n and n >= 59 and n <= 76)
    end
    return false
end

local function valid_ecl(v: string): boolean
    local colors = {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}
    for _, color in ipairs(colors) do
        if v == color then
            return true
        end
    end
    return false
end

local function valid(passport: common.Passport): boolean
    return not not (
        four_digits_between(passport.byr, "1920", "2002") and
        four_digits_between(passport.iyr, "2010", "2020") and
        four_digits_between(passport.eyr, "2020", "2030") and
        valid_hgt(passport.hgt) and
        passport.hcl and #(passport.hcl:match("^#[%da-f]+$") or "") == 7 and
        valid_ecl(passport.ecl) and
        passport.pid and passport.pid:match("^%d%d%d%d%d%d%d%d%d$")
    )
end

print(common.count_valid(valid))
