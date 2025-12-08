local util = require "util"

local M = {}

local record Box
    x: integer
    y: integer
    z: integer
end

local record Distance
    b1: integer
    b2: integer
    d2: integer
end

local function parse_input(input_fn: string): {Box}
    local lines = util.read_lines(input_fn)
    local r = {}
    for i, l in ipairs(lines) do
        local x, y, z = l:match("(%d+)%,(%d+),(%d+)")
        r[i] = {
            x = math.tointeger(tonumber(x)),
            y = math.tointeger(tonumber(y)),
            z = math.tointeger(tonumber(z)),
        }
    end
    return r
end


local function dcmp(d1: Distance, d2: Distance): boolean
    return d1.d2 < d2.d2
end


local function get_distances(boxes: {Box}): {Distance}
    local r = {}
    for i = 1, #boxes - 1 do
        for j = i + 1, #boxes do
            local b1, b2 = boxes[i], boxes[j]
            local d2 = (b1.x - b2.x)^2 + (b1.y - b2.y)^2 + (b1.z - b2.z)^2
            r[#r+1] = {b1 = i, b2 = j, d2 = math.tointeger(d2)}
        end
    end
    table.sort(r, dcmp)
    return r
end

local type Circuit = {integer:boolean}

local function make_circuits(distances: {Distance}, num_boxes: integer): {Circuit}, Distance |  nil
    local circuits: {Circuit} = {}
    for _, d in ipairs(distances) do
        local cb1, cb2 = 0, 0
        for i, c in ipairs(circuits) do
            if c[d.b1] then cb1 = i end
            if c[d.b2] then cb2 = i end
        end
        if cb1 == 0 and cb2 == 0 then
            circuits[#circuits + 1] = {[d.b1] = true, [d.b2]  = true}
        elseif cb1 == 0 then
            circuits[cb2][d.b1] = true
        elseif cb2 == 0 then
            circuits[cb1][d.b2] = true
        elseif cb1 ~= cb2 then
            util.set_add(circuits[cb1], circuits[cb2])
            table.remove(circuits, cb2)
        end
        if #circuits == 1 and util.set_size(circuits[1]) == num_boxes then
            return circuits, d
        end
    end
    return circuits, nil
end

function M.run(input_fn: string, part: integer) : integer
    local boxes = parse_input(input_fn)
    local distances = get_distances(boxes)
    if part == 1 then
        local short_distances = {}
        local max_distances = 1000
        if input_fn == "example.txt" then max_distances = 10 end
        for i = 1, max_distances do
            short_distances[i] = distances[i]
        end
        distances = short_distances
    end
    local circuits, last_distance = make_circuits(distances, #boxes)
    if part == 1 then
        assert(not last_distance)
        local circuit_sizes = util.map(circuits, util.set_size)
        table.sort(circuit_sizes, function(a: integer, b: integer): boolean return b < a  end)
        return circuit_sizes[1] * circuit_sizes[2] * circuit_sizes[3]
    end
    assert(last_distance)
    last_distance = last_distance as Distance
    return boxes[last_distance.b1].x * boxes[last_distance.b2].x
end

return M
