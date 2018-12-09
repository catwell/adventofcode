local util = require "util"

local function parse_lines(lines)
    local r = {}
    local ptrn = "Step (%w) must be finished before step (%w) can begin."
    for _, l in ipairs(lines) do
        local s, d = string.match(l, ptrn)
        if not r[s] then r[s] = {} end
        if not r[d] then r[d] = {} end
        r[d][s] = true
    end
    return r
end

local function step_duration(n)
    return (string.byte(n) - string.byte("A") + 61)
end

local deps = parse_lines(util.read_lines(arg[1]))

local function next_available_step()
    local r = {}
    for s, l in pairs(deps) do
        if not next(l) then
            table.insert(r, s)
        end
    end
    table.sort(r)
    return r[1]
end

local function work_still_available()
    return not not next(deps)
end

local function make_step_available(s)
    for _, l in pairs(deps) do
        l[s] = nil
    end
end

local function remove_step(s)
    deps[s] = nil
end

local S = { {t = 0}, {t = 0}, {t = 0}, {t = 0}, {t = 0} }

local function get_next_available_worker()
    for i = 1, #S do
        if S[i].t == 0 then
            return i
        end
    end
end

local function all_workers_available()
    for i = 1, #S do
        if S[i].t > 0 then
            return false
        end
    end
    return true
end

local function assign_work(w, s, t)
    S[w].s = s
    S[w].t = t
    remove_step(s)
end

local function work()
    for i = 1, #S do
        if S[i].t > 0 then
            S[i].t = S[i].t - 1
            if S[i].t == 0 then
                make_step_available(S[i].s)
            end
        end
    end
end

local t = 0
while true do
    while true do
        local w = get_next_available_worker()
        if w then
            local s = next_available_step()
            if s then
                assign_work(w, s, step_duration(s))
            else
                if (not work_still_available()) and all_workers_available() then
                    print(t)
                    return
                end
                break
            end
        else
            break
        end
    end
    t = t + 1
    work()
end
