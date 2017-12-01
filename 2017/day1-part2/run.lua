local function read_file(path)
    local f = assert(io.open(path, "rb"))
    local r = assert(f:read("*all"))
    f:close()
    return r
end

local s = read_file(arg[1]):gsub("[^%d]", "")

local n, l = 0, #s
assert(l % 2 == 0)
l = l // 2

for i = 1, l do
    if s:byte(i) == s:byte(i + l) then
        n = n + tonumber(s:sub(i, i))
    end
end

print(2 * n)
