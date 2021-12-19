local util = require "util"

local record State
   map: {string: {string: boolean}}
   position: string
   visited: {string: boolean}
   wildcard: boolean
end

local function parse_input() : {string: {string: boolean}}
    local map = {}
    for _, line in ipairs(util.read_lines("input.txt")) do
        local c1, c2 = line:match("^(%w+)-(%w+)$")
        if not map[c1] then map[c1] = {} end
        if not map[c2] then map[c2] = {} end
        map[c1][c2] = true
        map[c2][c1] = true
    end
    return map
end

local function islower(s: string): boolean
    return s >= 'a'
end

local function next_states(state: State): {State}
    local r = {}
    for position in pairs(state.map[state.position]) do
        if islower(position) then
            if not state.visited[position] then
                local visited = util.copy_table(state.visited)
                visited[position] = true
                local new_state = {
                    map = state.map,
                    visited = visited,
                    position = position,
                    wildcard = state.wildcard,
                }
                table.insert(r, new_state)
            elseif state.wildcard and position ~= 'start' then
                local visited = util.copy_table(state.visited)
                local new_state = {
                    map = state.map,
                    visited = visited,
                    position = position,
                    wildcard = false,
                }
                table.insert(r, new_state)
            end
        else
            local new_state = {
                map = state.map,
                visited = state.visited,
                position = position,
                wildcard = state.wildcard,
            }
            table.insert(r, new_state)
        end
    end
    return r
end

local M = {}

function M.run(part: integer) : integer
    local map = parse_input()
    local init_state = {
        map = map,
        visited = {start = true},
        position = 'start',
        wildcard = (part == 2),
    }
    local c = 0
    local states : {State} = {init_state}
    while #states > 0 do
        local new_states = {}
        for _, state in ipairs(states) do
            if state.position == 'end' then
                c = c + 1
            else
                util.extend_seq(new_states, next_states(state))
            end
        end
        states = new_states
    end
    return c
end

return M
