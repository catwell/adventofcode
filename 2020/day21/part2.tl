local common = require "common"
local util = require "util"

local entries = common.parse_input()
local allergizing_foods = common.get_allergizing_foods(entries)

local record Entry
    food: string
    allergen: string
end

local function cmp(a: Entry, b: Entry): boolean
    return a.allergen < b.allergen
end

local function extract_food(x: Entry): string
    return x.food
end

local t = {}
for k, v in pairs(allergizing_foods) do t[#t+1] = {food = k, allergen = v} end
table.sort(t, cmp)
print(table.concat(util.map(t, extract_food), ","))
