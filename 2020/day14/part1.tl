local common = require "common"

local machine = common.machine_from_input()

local function on_mem(self: common.Machine, mem: common.Mem)
    self.mem[mem.reg] = mem.val & self.mask.neg | self.mask.pos
end

machine:run_all(on_mem)

local r = 0
for _, v in pairs(machine.mem) do r = r + v end
print(r)
