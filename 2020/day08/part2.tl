local common = require "common"

local function try_switching_nop(tried_pos: {number:boolean}): boolean
    local machine = common.machine_from_input()
    local switched = false
    while true do
        local ins = machine.instructions[machine.pos]
        if not ins then
            print(machine.accumulator)
            return true
        end
        if not switched and not tried_pos[machine.pos] then
            if ins.code == "nop" then
                switched = true
                tried_pos[machine.pos] = true
                ins.code = "jmp"
            elseif ins.code == "jmp" then
                switched = true
                tried_pos[machine.pos] = true
                ins.code = "nop"
            end
        end
        if ins.executed then
            return false
        end
        machine:run_one()
    end
end

local tried_pos = {}
while true do
    local ok = try_switching_nop(tried_pos)
    if ok then return end
end
