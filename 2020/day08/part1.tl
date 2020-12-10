local common = require "common"

local machine = common.machine_from_input()
while true do
    local ins = machine.instructions[machine.pos]
    if ins.executed then
        print(machine.accumulator)
        return
    end
    machine:run_one()
end
