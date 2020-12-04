local common = require "common"

local function valid(passport: common.Passport): boolean
    local mandatory = {"byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"}
    for _, data in ipairs(mandatory) do
        if not passport[data] then
            return false
        end
    end
    return true
end

print(common.count_valid(valid))
