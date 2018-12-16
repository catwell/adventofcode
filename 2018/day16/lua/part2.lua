local util = require "util"
local common = require "common"

local input = common.parse_input(util.read_lines(arg[1]))
local machine = common.new_machine()

local opcodes = {}
for i = 0 , 15 do
    opcodes[i] = util.copy_table(machine.op)
end

-- eliminate invalid opcodes according to the sample
for _, s in ipairs(input.samples) do
    for name, op in pairs(opcodes[s.instruction[0]]) do
        machine.r = util.copy_table(s.before)
        op(machine, table.unpack(s.instruction, 1, 3))
        if not common.compare_registers(machine.r, s.after) then
            opcodes[s.instruction[0]][name] = nil
        end
    end
end

-- deduce all opcode values
while true do
    local known_opcodes = 0
    for i = 0, 15 do
        if util.count_keys(opcodes[i]) == 1 then
            local name = next(opcodes[i])
            for j = 0, 15 do
                if j ~= i then
                    opcodes[j][name] = nil
                end
            end
            known_opcodes = known_opcodes + 1
        end
    end
    if known_opcodes == 16 then break end
end

-- run the program
machine:reset()
for _, instruction in ipairs(input.program) do
    local _, op = next(opcodes[instruction[0]])
    op(machine, table.unpack(instruction, 1, 3))
end

print(machine.r[0])
