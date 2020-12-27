-- in `cups`, cups[i] is the label of the successor of cup with label `i`

local record State
    current: number
    cups: {number}
    cups_count: number
end

local M = { INPUT = {7, 8, 4, 2, 3, 5, 9, 1, 6} }

local function pick_one(state: State): number
    local v = state.cups[state.current]
    state.cups[state.current] = state.cups[v]
    state.cups[v] = nil
    return v
end

local function insert_one(state: State, label: number, pos: number)
    local n = state.cups[pos]
    state.cups[pos] = label
    state.cups[label] = n
end

local function find_destination(state: State): number
    local r = state.current
    while true do
        r = r - 1
        if r < 1 then r = state.cups_count end
        if state.cups[r] then return r end
    end
end

function M.move(state: State)
    local t = {}
    for i = 1, 3 do t[i] = pick_one(state) end
    local destination = find_destination(state)
    for i = 3, 1, -1 do insert_one(state, t[i], destination) end
    state.current = state.cups[state.current]
end


function M.new_state(input: {number}, count: number): State
    for i = #input + 1, count do
        input[i] = i
    end

    local cups = {}
    for i = 2, #input do
        cups[input[i - 1]] = input[i]
    end
    cups[input[#input]] = input[1]

    return {
        current = input[1],
        cups_count = count,
        cups = cups,
    }
end

return M
