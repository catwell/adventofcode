local util = require "util"

local record Entry
    foods: {string: boolean}
    allergens: {string: boolean}
end

local M = {}

local function parse_line(line: string) : Entry
    local p1, p2 = line:match("^([%w%s]+) %(contains ([%w%s,]+)%)$")
    return {
        foods = util.to_set(util.split(p1, " ")),
        allergens = util.to_set(util.split(p2, ", ")),
    }
end

function M.parse_input() : {Entry}
    local lines = util.read_lines("input.txt")
    return util.map(lines, parse_line)
end

function M.get_allergizing_foods(entries: {Entry}): {string: string}
    local m : {string: {string: boolean}} = {}
    for _, entry in ipairs(entries) do
        for allergen in pairs(entry.allergens) do
            if m[allergen] then
                util.set_intersect(m[allergen], entry.foods)
            else
                m[allergen] = util.copy_table(entry.foods)
            end
        end
    end

    local r : {string: string} = {}
    local empty = false
    while not empty do
        empty = true
        for allergen, foods in pairs(m) do
            empty = false
            util.set_subtract(foods, r)
            if util.count_keys(foods as {any: any}) == 1 then
                r[next(foods)] = allergen
                m[allergen] = nil
            end
        end
    end
    return r
end

return M
