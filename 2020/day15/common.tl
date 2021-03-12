local util = require "util"

local record Spoken
    last: integer
    prev: integer
end

local record State
    spoken: {integer:Spoken}
    turn: integer
    last_spoken: integer
    speak: function(State, integer)
    play: function(State)
end

local M = {}

local function parse_input() : {integer}
    return util.parse_integers(util.read_file("input.txt"))
end

local function speak(self: State, n: integer)
    self.last_spoken = n
    self.turn = self.turn + 1
    if not self.spoken[n] then
        self.spoken[n] = {}
    end
    self.spoken[n].prev = self.spoken[n].last
    self.spoken[n].last = self.turn
end

local function play(self: State)
    if self.spoken[self.last_spoken].prev then
        local spoken = self.spoken[self.last_spoken]
        self:speak(spoken.last - spoken.prev)
    else
        self:speak(0)
    end
end

local state_mt = {__index = {speak = speak, play = play}}

local function new_state(initial: {integer}): State
    local r : State = setmetatable({
        spoken = {},
        turn = 0,
    }, state_mt)
    for _, n in ipairs(initial) do
        r:speak(n)
    end
    return r
end

function M.num_at(n: integer) : integer
    local input = parse_input()
    local st = new_state(input)
    while st.turn < n do
        st:play()
    end
    return st.last_spoken
end

return M
