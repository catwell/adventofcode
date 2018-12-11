local M = {}

function M.parse_input(s)
    local ptrn = "(%d+) players; last marble is worth (%d+) points"
    local c, n = string.match(s, ptrn)
    return tonumber(c), tonumber(n)
end

local game_mod = "game_circlist"
if os.getenv("USE_DEQUE") then
    print("WITH DEQUE")
    game_mod = "game_deque"
end
M.new_game = assert(require(game_mod).new_game)

return M
