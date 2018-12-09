local util = require "util"

local function parse_numbers(s)
    local t = {}
    local function f(x) table.insert(t, tonumber(x)) end
    s:gsub("%w+", f)
    return t
end

local nums = parse_numbers(util.read_file(arg[1]))

local function parse_node(idx)
    local r = { index = idx, child = {}, value = 0 }
    r.num_children, r.num_meta = nums[idx], nums[idx + 1]
    idx = idx + 2
    for i = 1, r.num_children do
        r.child[i] = parse_node(idx)
        idx = r.child[i].next_index
    end
    if r.num_children == 0 then
        for _ = 1, r.num_meta do
            r.value = r.value + nums[idx]
            idx = idx + 1
        end
    else
        for _ = 1, r.num_meta do
            if nums[idx] <= r.num_children then
                r.value = r.value + r.child[nums[idx]].value
            end
            idx = idx + 1
        end
    end
    r.next_index = idx
    return r
end

print(parse_node(1).value)
