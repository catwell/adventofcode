local R = {}

local ops = {
    [">"] = function(a, b) return a > b end,
    ["<"] = function(a, b) return a < b end,
    [">="] = function(a, b) return a >= b end,
    ["<="] = function(a, b) return a <= b end,
    ["=="] = function(a, b) return a == b end,
    ["!="] = function(a, b) return a ~= b end,
}

local function process(l)
    local r1, i, n1, r2, c, n2 = l:match(
        "(%w+) (%w+) ([0-9-]+) if (%w+) ([<>=!]+) ([0-9-]+)"
    )

    n1 = assert(tonumber(n1))
    n2 = assert(tonumber(n2))

    if ops[c](R[r2] or 0, n2) then
        if i == "inc" then
            R[r1] = (R[r1] or 0) + n1
        else
            assert(i == "dec")
            R[r1] = (R[r1] or 0) - n1
        end
    end
end

for l in io.lines(arg[1]) do process(l) end

local m = -math.huge
for _, v in pairs(R) do
    if v > m then m = v end
end
print(m)
