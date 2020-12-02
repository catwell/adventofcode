package.path = package.path .. ";../?.lua"
local util = require "util"

local input = util.read_file("input.txt")

local t = {}

for v in input:gmatch("%d+") do
    table.insert(t, tonumber(v))
end

for i = 1, #t do
    for j = i + 1, #t do
        if t[i] + t[j] == 2020 then
            print(t[i] * t[j])
        end
    end
end
