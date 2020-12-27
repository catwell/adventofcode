local util = require "util"

local record Mem
   reg: number
   val: number
end

local record Mask
    pos: number
    neg: number
    flt: {number}
end

local enum InsType
    "mask"
    "mem"
end

local record Instruction
    itype: InsType
    val: any
end

local type MemExec = function(Machine, Mem)

local record Machine
    instructions: {Instruction}
    mask: Mask
    pos: number
    mem: {number:number}
    run_one: function(Machine, MemExec)
    run_all: function(Machine, MemExec)
end

local M = {Machine = Machine, Mem = Mem}

local function parse_mask(l: string): Mask
    local m = l:match("^mask = ([X01]+)$")
    local r = {pos = 0, neg = 0, flt = {}}
    local n = 1
    for p = #m, 1, -1 do
        local v = m:sub(p, p)
        if v ~= "0" then r.neg = r.neg + n end
        if v == "1" then r.pos = r.pos + n end
        if v == "X" then r.flt[#r.flt + 1] = n end
        n = n * 2
    end
    return r
end

local function parse_instruction(l: string) : Instruction
    local reg, v = l:match("^mem%[(%d+)%] = (%d+)$")
    if reg then
        local val: Mem = {reg = tonumber(reg), val = tonumber(v)}
        return {itype = "mem", val = val}
    else
        return {itype = "mask", val = parse_mask(l)}
    end
end

local function parse_input() : {Instruction}
    local lines = util.read_lines("input.txt")
    return util.map(lines, parse_instruction)
end

local function run_one(self: Machine, on_mem: MemExec)
    local ins = self.instructions[self.pos]
    if ins.itype == "mask" then
        self.mask = ins.val as Mask
    else
        on_mem(self, ins.val as Mem)
    end
    self.pos = self.pos + 1
end

local function run_all(self: Machine, on_mem: MemExec)
    while self.instructions[self.pos] do
        self:run_one(on_mem)
    end
end

local machine_mt = {__index = {run_one = run_one, run_all = run_all}}

local function new_machine(instructions: {Instruction}): Machine
    return setmetatable({
        instructions = instructions,
        pos = 1,
        mem = {},
    }, machine_mt)
end

function M.machine_from_input(): Machine
    return new_machine(parse_input())
end

return M
