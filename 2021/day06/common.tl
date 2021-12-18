local util = require "util"

local record State
   counts: {integer:integer}
   tick: function(State)
   total: function(State): integer
end

local M = {}

local function parse_input() : {integer}
    return util.parse_integers(util.read_file("input.txt"))
end

local function state_tick(state: State)
    local counts = {}
    for i = 0, 7 do counts[i] = state.counts[i + 1] end
    counts[6] = counts[6] + state.counts[0]
    counts[8] = state.counts[0]
    state.counts = counts
end

local function state_total(state: State): integer
    local r = 0
    for i = 0, 8 do r = r + state.counts[i] end
    return r
end

local function state_new(input: {integer}): State
    local counts = {}
    for i = 0, 8 do counts[i] = 0 end
    for _, n in ipairs(input) do counts[n] = counts[n] + 1 end
    return {
        counts = counts,
        tick = state_tick,
        total = state_total,
    }
end

function M.run(part: integer) : integer
    local state = state_new(parse_input())
    local n = (part == 2) and 256 or 80
    for _ = 1, n do
        state:tick()
    end
    return (state:total())
end

return M
