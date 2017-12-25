local TAPE = {}
local POS = { cur = 0, min = 0, max = 0 }

local function left()
    POS.cur = POS.cur - 1
    if POS.cur < POS.min then POS.min = POS.cur end
end

local function right()
    POS.cur = POS.cur + 1
    if POS.cur > POS.max then POS.max = POS.cur end
end

local function is0()
    return not TAPE[POS.cur]
end

local function set0()
    TAPE[POS.cur] = nil
end

local function set1()
    TAPE[POS.cur] = true
end

local function checksum()
    local c = 0
    for i = POS.min, POS.max do
        if TAPE[i] then c = c + 1 end
    end
    return c
end

local stepc, stateA, stateB, stateC, stateD, stateE, stateF

local function step(state)
    stepc = stepc - 1
    if stepc >= 0 then return state() end
end

stateA = function()
    if is0() then
        set1()
        right()
        return step(stateB)
    else
        set0()
        right()
        return step(stateC)
    end
end

stateB = function()
    if is0() then
        left()
        return step(stateA)
    else
        set0()
        right()
        return step(stateD)
    end
end

stateC = function()
    if is0() then
        set1()
        right()
        return step(stateD)
    else
        right()
        return step(stateA)
    end
end

stateD = function()
    if is0() then
        set1()
        left()
        return step(stateE)
    else
        set0()
        left()
        return step(stateD)
    end
end

stateE = function()
    if is0() then
        set1()
        right()
        return step(stateF)
    else
        left()
        return step(stateB)
    end
end

stateF = function()
    if is0() then
        set1()
        right()
        return step(stateA)
    else
        right()
        return step(stateE)
    end
end

stepc = 12399302
step(stateA)
print(checksum())
