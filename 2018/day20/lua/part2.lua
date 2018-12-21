local util = require "util"
local common = require "common"

local input = util.read_file(arg[1])
local regex = common.parse_regexp(input)

local state = common.new_state()
common.longest_path(regex, state) -- to crawl

local count = 0
for _, l in pairs(state.visited_first) do
    for _, v in pairs(l) do
        if v >= 1000 then
            count = count + 1
        end
    end
end
print(count)
