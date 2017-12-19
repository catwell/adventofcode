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

function M.add(self, x, y)
    self.reg[x] = self.reg[x] + self:val(y)
end

function M.mul(self, x, y)
    self.reg[x] = self.reg[x] * self:val(y)
end

function M.mod(self, x, y)
    self.reg[x] = self.reg[x] % self:val(y)
end

function M.jgz(self, x, y)
    if self:val(x) > 0 then
        self.pos = self.pos + self:val(y) - 1
    end
end

function M.enqueue(self, v)
    table.insert(self.queue, v)
end

function M.dequeue(self)
    return table.remove(self.queue, 1)
end

function M.snd(self, x)
    self.sent_count = self.sent_count + 1
    self.partner:enqueue(self:val(x))
end

function M.rcv(self, x)
    local v = self:dequeue()
    if v then
        self.reg[x] = v
    else
        self.blocked = true
    end
end

function M.process(self, l)
    local op, x, y = l:match("(%w+) ([%w%d-]+) ?([%w%d-]*)")
    self[op](self, x, y)
end

function M.next(self)
    self.blocked = false
    self:process(ops[self.pos])
    if not self.blocked then
        self.pos = self.pos + 1
    end
end

local function new(n)
    local self = { n = n, pos = 1, queue = {}, sent_count = 0 }
    self.reg = setmetatable({p = n}, {__index = function() return 0 end})
    return setmetatable(self, {__index = M})
end

local p0, p1 = new(0), new(1)
p0.partner, p1.partner = p1, p0

while true do
    p0:next()
    if p0.blocked then
        p1:next()
        if p1.blocked then
            print(p1.sent_count)
            return
        end
    end
end
