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

for i = 1, #s do
    if not seen[s[i].program] then
        print(s[i].program)
        return
    end
end
