local util = require "util"

local N = 5 -- board size

local record Cell
   value: integer
   checked: boolean
end

local record Board
    cells: {{Cell}}
    won: boolean
    play: function(Board, integer)
    check_win: function(Board, integer, integer): boolean
end

local record State
   draws: {integer}
   boards: {Board}
   turn: integer
   won: integer
   last_won: integer
   play: function(State)
end

local M = {}

local function board_check_win(board: Board, row: integer, col: integer): boolean
    local n = 0
    for i = 1, N do
        if not board.cells[row][i].checked then break end
        n = n +1
    end
    if n == 5 then return true end
    n = 0
    for i = 1, N do
        if not board.cells[i][col].checked then break end
        n = n +1
    end
    return n == 5
end


local function board_play(board: Board, v: integer)
    for row = 1, N do
        for col = 1, N do
            local cell = board.cells[row][col]
            if cell.value == v then
                cell.checked = true
                if board:check_win(row, col) then
                    board.won = true
                end
            end
        end
    end
end

local function state_play(state: State)
    state.turn = state.turn + 1
    local v = state.draws[state.turn]
    for idx, board in ipairs(state.boards) do
        if not board.won then
            board:play(v)
            if board.won then
                state.won = state.won + 1
                state.last_won = idx
            end
        end
    end
end

local function new_cell(v: integer): Cell
    return {
        value = v,
        checked = false,
    }
end

local function parse_input(): State
    local lines = util.read_lines("input.txt")
    local draws = util.parse_integers(lines[1])
    local boards = {}
    local pos = 3
    while true do
        if not lines[pos] then break end
        local board = {
            cells = {},
            won = false,
            play = board_play,
            check_win = board_check_win,
        }
        for i = 1, N do
            board.cells[i] = util.map(util.parse_integers(lines[pos]), new_cell)
            pos = pos + 1
        end
        pos = pos + 1
        table.insert(boards, board)
    end
    return {
        draws = draws,
        boards = boards,
        turn = 0,
        won = 0,
        play = state_play,
    }
end

function M.run(part: integer) : integer
    local state = parse_input()
    local match = 1
    if part == 2 then match = #state.boards end
    while state.won < match do
        state:play()
    end
    local board = state.boards[state.last_won]
    local c = 0
    for row = 1, N do
        for col = 1, N do
            local cell = board.cells[row][col]
            if not cell.checked then
                c = c + board.cells[row][col].value
            end
        end
    end
    return c * state.draws[state.turn]
end

return M
