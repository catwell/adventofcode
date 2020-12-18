local util = require "util"

local record Spoken
    last: number
    prev: number
end

local record State
    spoken: {number:Spoken}
    turn: number
    last_spoken: number
    speak: function(State, number)
    play: function(State)
end

local M = {}

local function parse_input() : {number}
    return util.parse_integers(util.read_file("input.txt"))
end

local function speak(self: State, n: number)
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

local function new_state(initial: {number}): State
    local r : State = setmetatable({
        spoken = {},
        turn = 0,
    }, state_mt)
    for _, n in ipairs(initial) do
        r:speak(n)
    end
    return r
end

function M.num_at(n: number) : number
    local input = parse_input()
    local st = new_state(input)
    while st.turn < n do
        st:play()
    end
    return st.last_spoken
end

return M
