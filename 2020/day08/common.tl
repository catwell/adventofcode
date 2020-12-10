local util = require "util"
local fmt = string.format

local record Instruction
   executed: boolean
   code: string
   operand: number
end

local record Machine
    instructions: {Instruction}
    accumulator: number
    pos: number
    run_one: function(Machine)
end

local M = {}

local function parse_line(line: string) : Instruction
    local code, operand = string.match(line, "(%w+) ([+-]%d+)")
    return {
        executed = false,
        code = code,
        operand = tonumber(operand),
    }
end

local function parse_input() : {Instruction}
    return util.map(util.read_lines("input.txt"), parse_line)
end

local function run_one(self: Machine)
    local ins = self.instructions[self.pos]
    if ins.code == "nop" then
        self.pos = self.pos + 1
    elseif ins.code == "acc" then
        self.accumulator = self.accumulator + ins.operand
        self.pos = self.pos + 1
    elseif ins.code == "jmp" then
        self.pos = self.pos + ins.operand
    else
        error(fmt("invalid instruction %s", ins.code))
    end
    ins.executed = true
end

local machine_mt = {__index = {run_one = run_one}}

local function new_machine(instructions: {Instruction}): Machine
    return setmetatable({
        instructions = instructions,
        accumulator = 0,
        pos = 1,
    }, machine_mt)
end

function M.machine_from_input(): Machine
    return new_machine(parse_input())
end

return M
