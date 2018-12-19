local M = {}

function M.parse_input(lines)
    local r = { program = {} }
    r.ip_reg = tonumber(lines[1]:match("#ip (%d+)"))
    local ptrn, p = "(%w+) (%d+) (%d+) (%d+)", 2
    while lines[p] do
        local ins, a, b, c = lines[p]:match(ptrn)
        if not ins then break end
        r.program[p - 2] = { ins, tonumber(a), tonumber(b), tonumber(c) }
        p = p + 1
    end
    return r
end

local MM = { op = {} }

function MM.reset(self)
    self.r = {[0] = 0, [1] = 0, [2] = 0, [3] = 0, [4] = 0, [5] = 0}
end

function MM.op.addr(self, A, B, C)
    self.r[C] = self.r[A] + self.r[B]
end

function MM.op.addi(self, A, B, C)
    self.r[C] = self.r[A] + B
end

function MM.op.mulr(self, A, B, C)
    self.r[C] = self.r[A] * self.r[B]
end

function MM.op.muli(self, A, B, C)
    self.r[C] = self.r[A] * B
end

function MM.op.banr(self, A, B, C)
    self.r[C] = self.r[A] & self.r[B]
end

function MM.op.bani(self, A, B, C)
    self.r[C] = self.r[A] & B
end

function MM.op.borr(self, A, B, C)
    self.r[C] = self.r[A] | self.r[B]
end

function MM.op.bori(self, A, B, C)
    self.r[C] = self.r[A] | B
end

function MM.op.setr(self, A, _, C)
    self.r[C] = self.r[A]
end

function MM.op.seti(self, A, _, C)
    self.r[C] = A
end

function MM.op.gtir(self, A, B, C)
    self.r[C] = (A > self.r[B]) and 1 or 0
end

function MM.op.gtri(self, A, B, C)
    self.r[C] = (self.r[A] > B) and 1 or 0
end

function MM.op.gtrr(self, A, B, C)
    self.r[C] = (self.r[A] > self.r[B]) and 1 or 0
end

function MM.op.eqir(self, A, B, C)
    self.r[C] = (A == self.r[B]) and 1 or 0
end

function MM.op.eqri(self, A, B, C)
    self.r[C] = (self.r[A] == B) and 1 or 0
end

function MM.op.eqrr(self, A, B, C)
    self.r[C] = (self.r[A] == self.r[B]) and 1 or 0
end

function MM.run_op(self, op, A, B, C)
    self.r[self.ip_reg] = self.ip
    self.op[op](self, A, B, C)
    self.ip = self.r[self.ip_reg] + 1
end

function MM.run_program(self)
    while true do
        local op = self.program[self.ip]
        if not op then break end
        self.run_op(self, table.unpack(op))
    end
end

function M.new_machine(ip_reg, program)
    local self = { ip = 0, ip_reg = ip_reg, program = program }
    MM.reset(self)
    return setmetatable(self, {__index = MM})
end

return M
