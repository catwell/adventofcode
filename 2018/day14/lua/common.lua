local M = {}

function M.initial_state()
    local r = { p1 = 0, p2 = 1, n = 2 }
    r.s = { [0] = 3, [1] = 7 }
    return r
end

function M.tick(state)
    local n = state.s[state.p1] + state.s[state.p2]
    if n > 9 then
        state.s[state.n] = n // 10
        state.n = state.n + 1
    end
    state.s[state.n] = n % 10
    state.n = state.n + 1
    state.p1 = (state.p1 + state.s[state.p1] + 1) % state.n
    state.p2 = (state.p2 + state.s[state.p2] + 1) % state.n
end

return M
