local ops = {}

for l in io.lines(arg[1]) do table.insert(ops, l) end

local M = {}

function M.val(self, x)
    if x:match("[a-z]") then
        return self.reg[x]
    else
        return tonumber(x)
    end
end

function M.set(self, x, y)
    self.reg[x] = self:val(y)
end

function M.sub(self, x, y)
    self.reg[x] = self.reg[x] - self:val(y)
end

function M.mul(self, x, y)
    self.reg[x] = self.reg[x] * self:val(y)
end

function M.jnz(self, x, y)
    if self:val(x) ~= 0 then
        self.pos = self.pos + self:val(y) - 1
    end
end

function M.process(self, l)
    local op, x, y = l:match("(%w+) ([%w%d-]+) ?([%w%d-]*)")
    self[op](self, x, y)
end

function M.next(self)
    if not ops[self.pos] then
        self.alive = false
        return
    end
    self:process(ops[self.pos])
    self.pos = self.pos + 1
end

local function new()
    local self = { pos = 1, alive = true }
    self.reg = setmetatable({}, {__index = function() return 0 end})
    return setmetatable(self, {__index = M})
end

local p = new()

local c = 0
p.mul = function(...)
    c = c + 1
    M.mul(...)
end

while true do
    if p.alive then
        p:next()
    else
        break
    end
end

print(c)
