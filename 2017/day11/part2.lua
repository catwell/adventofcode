local GRID

local function get_cell(y, x)
    local c = GRID[x][y]
    if not c.x then c.x = x end
    if not c.y then c.y = y end
    assert(c.x == x and c.y == y)
    return c
end

local function neighbors(self)
    return {
        self:to_n(),
        self:to_ne(),
        self:to_se(),
        self:to_s(),
        self:to_sw(),
        self:to_nw(),
    }
end

local cell_mt = {
    __index = {
        to_n = function(self) return get_cell(self.y + 2, self.x) end,
        to_s = function(self) return get_cell(self.y - 2, self.x) end,
        to_ne = function(self) return get_cell(self.y + 1, self.x + 1) end,
        to_se = function(self) return get_cell(self.y - 1, self.x + 1) end,
        to_nw = function(self) return get_cell(self.y + 1, self.x - 1) end,
        to_sw = function(self) return get_cell(self.y - 1, self.x - 1) end,
        neighbors = neighbors,
    }
}

local function deft_t()
    return setmetatable({}, {
        __index = function(t, k)
            t[k] = setmetatable({}, cell_mt)
            return t[k]
        end
    })
end

local function deft_t2()
    return setmetatable({}, {
        __index = function(t, k)
            t[k] = deft_t()
            return t[k]
        end
    })
end

GRID = deft_t2()
local ORIGIN = get_cell(0, 0)

local function find_destination()
    local cur = get_cell(0, 0)
    cur.visited_1 = true
    local function process(dir)
        cur = cur["to_" .. dir](cur)
        cur.visited_1 = true
    end
    local util = require "util"
    util.read_file(arg[1]):gsub("%w+", process)
    return cur
end

local function steps_to_furthest()
    local steps, l, nl = 0, {ORIGIN}, {}
    ORIGIN.visited = true
    while true do
        assert(#l >= 1)
        for _, c in ipairs(l) do
            for _, n in ipairs(c:neighbors()) do
                if not n.visited then
                    n.visited = true
                    nl[#nl+1] = n
                end
            end
        end
        local found = false
        for _, c in ipairs(nl) do
            if c.visited_1 then
                found = true
                break
            end
        end
        if not found then
            return steps
        end
        steps = steps + 1
        l, nl = nl, {}
    end
end

find_destination()
print(steps_to_furthest())
