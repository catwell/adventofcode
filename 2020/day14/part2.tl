local common = require "common"

local machine = common.machine_from_input()
local all_ones = math.floor(2^36 - 1)

local function on_mem(self: common.Machine, mem: common.Mem)
    local reg_base = mem.reg | self.mask.pos
    local regs = {reg_base}
    for _, n in ipairs(self.mask.flt) do
        local new_regs = {}
        for _, reg in ipairs(regs) do
            new_regs[#new_regs + 1] = reg | n
            new_regs[#new_regs + 1] = reg & (all_ones - n)
        end
        regs = new_regs
    end
    for _, reg in ipairs(regs) do
        self.mem[reg] = mem.val
    end
end

machine:run_all(on_mem)

local r = 0
for _, v in pairs(machine.mem) do r = r + v end
print(r)
