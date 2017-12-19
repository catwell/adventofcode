local REG = setmetatable({}, {__index = function() return 0 end})
local OP = {}

local function val(x)
    if x:match("[a-z]") then
        return REG[x]
    else
        return tonumber(x)
    end
end

function OP.set(x, y)
    REG[x] = val(y)
end

function OP.add(x, y)
    REG[x] = REG[x] + val(y)
end

function OP.mul(x, y)
    REG[x] = REG[x] * val(y)
end

function OP.mod(x, y)
    REG[x] = REG[x] % val(y)
end

local pos, ops, last = 1, {}, nil


function OP.snd(x)
    last = val(x)
end

function OP.rcv(x)
    local v = val(x)
    if v ~= 0 then
        print(last)
        os.exit()
    end
end

function OP.jgz(x, y)
    if val(x) > 0 then
        pos = pos + val(y) - 1
    end
end

local function process(l)
    local op, x, y = l:match("(%w+) ([%w%d-]+) ?([%w%d-]*)")
    OP[op](x, y)
end

for l in io.lines(arg[1]) do table.insert(ops, l) end

while true do
    process(ops[pos])
    pos = pos + 1
end
