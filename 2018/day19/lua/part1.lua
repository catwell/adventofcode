local util = require "util"
local common = require "common"

local input = common.parse_input(util.read_lines(arg[1]))
local machine = common.new_machine(input.ip_reg, input.program)
machine:run_program()

print(machine.r[0])
