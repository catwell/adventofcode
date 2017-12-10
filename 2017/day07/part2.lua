local function parse(l)
    local n, w = l:match("(%w+) %((%d+)%)")
    local _, p2 = l:match("(.*) %-> (.*)")
    local t = {}
    if p2 then
        p2:gsub("[a-z]+", function(x) t[#t+1] = x end)
    end
    return {
        program = n,
        weight = tonumber(w),
        above = t,
    }
end

local s = {}
for l in io.lines(arg[1]) do
    table.insert(s, parse(l))
end

local seen = {}
for i = 1, #s do
    for _, x in ipairs(s[i].above) do
        seen[x] = true
    end
end

local bottom
for i = 1, #s do
    if not seen[s[i].program] then
        bottom = s[i].program
        break
    end
end

local m = {}
for i = 1, #s do
    m[s[i].program] = s[i]
end

local function deal_with_unbalanced(n)
    local ln = m[n].above
    assert(#ln >= 3)
    local l = {}
    for i = 1, #ln do l[i] = m[ln[i]] end
    table.sort(
        l, function(a, b) return a.total_weight < b.total_weight end
    )
    if l[1].total_weight < l[2].total_weight then
        local diff = l[2].total_weight - l[1].total_weight
        print(l[1].weight + diff)
    else
        local diff = l[#l].total_weight - l[2].total_weight
        assert(l[#l].total_weight > l[2].total_weight)
        assert(diff <= l[#l].weight)
        print(l[#l].weight - diff)
    end
    return
end

local function check_balanced(n)
    if not m[n].total_weight then
        local tw = 0
        for _, n2 in ipairs(m[n].above) do
            if not check_balanced(n2) then return end
            tw = tw + m[n2].total_weight
        end
        if #m[n].above > 0 then
            local ideal_w = tw // #m[n].above
            for _, n2 in ipairs(m[n].above) do
                if m[n2].total_weight ~= ideal_w then
                    deal_with_unbalanced(n)
                    return
                end
            end
        end
        m[n].total_weight = tw + m[n].weight
    end
    return true
end

check_balanced(bottom)
