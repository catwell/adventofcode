local util = require "util"
local common = require "common"

local input = common.parse_input(util.read_lines(arg[1]))
local machine = common.new_machine()

local count = 0
for _, s in ipairs(input.samples) do
    local c = 0
    for _, op in pairs(machine.op) do
        machine.r = util.copy_table(s.before)
        op(machine, table.unpack(s.instruction, 1, 3))
        if common.compare_registers(machine.r, s.after) then
            c = c + 1
            if c == 3 then
                count = count + 1
                break
            end
        end
    end
end

print(count)
