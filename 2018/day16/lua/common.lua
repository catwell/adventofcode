local util = require "util"

local M = {}

function M.parse_input(lines)
    local r, p = { samples = {}, program = {} }, 1
    while true do
        if not lines[p]:match("Before") then
            break
        end
        table.insert(r.samples, {
            before = util.parse_integers(lines[p], 0),
            instruction = util.parse_integers(lines[p + 1], 0),
            after = util.parse_integers(lines[p + 2], 0),
        })
        p = p + 4
    end
    while lines[p] do
        local l = util.parse_integers(lines[p], 0)
        if l[1] then table.insert(r.program, l) end
        p = p + 1
    end
    return r
end

local MM = { op = {} }

function MM.reset(self)
    self.r = {[0] = 0, [1] = 0, [2] = 0, [3] = 0}
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

function M.new_machine()
    local self = {}
    MM.reset(self)
    return setmetatable(self, {__index = MM})
end

function M.compare_registers(r1, r2)
    for i = 0, 3 do
        if r1[i] ~= r2[i] then
            return false
        end
    end
    return true
end

return M
