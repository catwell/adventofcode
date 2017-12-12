local MAP = {}

local function process_line(l)
    local p, tail = l:match("^(%d+) %<%-%> (.*)")
    p = tonumber(p)
    if not MAP[p] then MAP[p] = {} end
    local function f(x) table.insert(MAP[p], tonumber(x)) end
    tail:gsub("%d+", f)
end

for l in io.lines(arg[1]) do
    process_line(l)
end

local c, s, l, nl = 0, {}, {0}, {}

while true do
    for _, x in ipairs(l) do
        for _, p in ipairs(MAP[x]) do
            if not s[p] then
                c = c + 1
                s[p] = true
                nl[#nl+1] = p
            end
        end
    end
    if #nl == 0 then break end
    l, nl = nl, {}
end

print(c)
