local util = require "util"

local function parse_numbers(s)
    local t = {}
    local function f(x) table.insert(t, tonumber(x)) end
    s:gsub("%w+", f)
    return t
end

local nums = parse_numbers(util.read_file(arg[1]))
local meta_sum = 0

local function parse_node(idx)
    local r = { index = idx, child = {} }
    r.num_children, r.num_meta = nums[idx], nums[idx + 1]
    idx = idx + 2
    for i = 1, r.num_children do
        r.child[i] = parse_node(idx)
        idx = r.child[i].next_index
    end
    for _ = 1, r.num_meta do
        meta_sum = meta_sum + nums[idx]
        idx = idx + 1
    end
    r.next_index = idx
    return r
end

parse_node(1)
print(meta_sum)
