local util = require "util"

local enum Axis
    "x"
    "y"
end

local record Instruction
    direction: Axis
    value: integer
end

local record State
    matrix: util.matrix.Matrix<string>
    instructions: {Instruction}
end

local function parse_input() : State
    local lines = util.read_lines("input.txt")
    local matrix = util.matrix.new(' ')
    local instructions = {}
    for _, line in ipairs(lines) do
        local x, y = line:match("^(%d+),(%d+)$")
        if y then
            -- y is the row, x is the column
            matrix:set(math.tointeger(y), math.tointeger(x), '#')
        else
            local direction, value = line:match("^fold along ([xy])=(%d+)$")
            if value then
                local ins = {
                    direction = direction as Axis,
                    value = math.tointeger(value),
                }
                table.insert(instructions, ins)
            end
        end
    end
    return {
        matrix = matrix,
        instructions = instructions,
    }
end

local function state_fold(state: State)
    local instruction = table.remove(state.instructions, 1)
    local matrix = util.matrix.new(' ')
    if instruction.direction == 'y' then
        for row = 0, instruction.value - 1 do
            for col = 0, state.matrix.max_col do
                matrix:set(row, col, state.matrix:get(row, col))
            end
        end
        for row = instruction.value + 1, state.matrix.max_row do
            local mirror = 2 * instruction.value - row
            for col = 0, state.matrix.max_col do
                if state.matrix:get(row, col) == '#' then
                    matrix:set(mirror, col, '#')
                end
            end
        end
    else
        for row = 0, state.matrix.max_row do
            for col = 0, instruction.value - 1 do
                matrix:set(row, col, state.matrix:get(row, col))
            end
        end
        for row = 0, state.matrix.max_row do
            for col = instruction.value + 1, state.matrix.max_col do
                if state.matrix:get(row, col) == '#' then
                    local mirror = 2 * instruction.value - col
                    matrix:set(row, mirror, '#')
                end
            end
        end
    end
    state.matrix = matrix
end

local function part1(state: State): integer
    state_fold(state)
    return state.matrix:count('#')
end

local function part2(state: State): integer
    while #state.instructions > 0 do
        state_fold(state)
    end
    for row = 0, state.matrix.max_row do
        for col = 0, state.matrix.max_col do
            io.stdout:write(state.matrix:get(row, col))
        end
        io.stdout:write("\n")
    end
    return 0
end

local M = {}

function M.run(part: integer) : integer
    local state = parse_input()
    if part == 1 then
        return part1(state)
    else
        return part2(state)
    end
end

return M
