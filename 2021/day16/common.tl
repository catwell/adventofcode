local util = require "util"

local record Packet
    version: integer
    type_id: integer
    literal: integer
    packets: {Packet}
    version_sum: function(Packet): integer
    value: function(Packet): integer
end

local function packet_version_sum(packet: Packet): integer
    local r = packet.version
    if packet.packets then
        for _, p in ipairs(packet.packets) do
            r = r + p:version_sum()
        end
    end
    return r
end

local function packet_value(packet: Packet): integer
    local r = 0
    if packet.type_id == 0 then -- sum
        for _, p in ipairs(packet.packets) do
            r = r + p:value()
        end
    elseif packet.type_id == 1 then -- product
        r = 1
        for _, p in ipairs(packet.packets) do
            r = r * p:value()
        end
    elseif packet.type_id == 2 then -- min
        r = math.maxinteger
        for _, p in ipairs(packet.packets) do
            local v = p:value()
            if v < r then r = v end
        end
    elseif packet.type_id == 3 then -- max
        r = 0
        for _, p in ipairs(packet.packets) do
            local v = p:value()
            if v > r then r = v end
        end
    elseif packet.type_id == 4 then -- literal
        r = packet.literal
    elseif packet.type_id == 5 then -- gt
        r = packet.packets[1]:value() > packet.packets[2]:value() and 1 or 0
    elseif packet.type_id == 6 then -- lt
        r = packet.packets[1]:value() < packet.packets[2]:value() and 1 or 0
    elseif packet.type_id == 7 then -- eq
        r = packet.packets[1]:value() == packet.packets[2]:value() and 1 or 0
    end
    return r
end

local packet_mt = {__index = {
    version_sum = packet_version_sum,
    value = packet_value,
}}

local function packet_new(p: Packet): Packet
    return setmetatable(p, packet_mt)
end

local record Stream
    bits: {integer}
    pos: integer
    read_bool: function(Stream): boolean
    read_int: function(Stream, integer): integer
    read_literal: function(Stream): integer
    parse: function(Stream): Packet
end

local function stream_read_bool(stream: Stream): boolean
    stream.pos = stream.pos + 1
    return stream.bits[stream.pos - 1] == 1
end

local function stream_read_int(stream: Stream, n: integer): integer
    local r = 0
    for i = 1, n do
        r = r | stream.bits[stream.pos + i - 1] << (n - i)
    end
    stream.pos = stream.pos + n
    return r
end

local function stream_read_literal(stream: Stream): integer
    local r, cont = 0, true
    while cont do
        cont = stream:read_bool()
        r = (r << 4) + stream:read_int(4)
    end
    return r
end

local function stream_parse(stream: Stream): Packet
    local version = stream:read_int(3)
    local type_id = stream:read_int(3)
    if type_id == 4 then
        return packet_new({
            version = version,
            type_id = 4,
            literal = stream:read_literal(),
        })
    end
    local length_type_id = stream:read_bool()
    local packets = {}
    if length_type_id then
        local length = stream:read_int(11)
        for i = 1, length do
            packets[i] = stream:parse()
        end
    else
        local length = stream:read_int(15)
        local p0 = stream.pos
        while stream.pos < p0 + length do
            packets[#packets + 1] = stream:parse()
        end
    end
    return packet_new({
        version = version,
        type_id = type_id,
        packets = packets,
    })
end

local function parse_input() : Stream
    local s = util.read_file("input.txt")
    local bits = {}
    for p = 1, #s do
        local n = tonumber(s:sub(p, p), 16)
        if n then
            local p2 = 4*(p-1) + 1
            bits[p2 + 0] = n >> 3 & 1
            bits[p2 + 1] = n >> 2 & 1
            bits[p2 + 2] = n >> 1 & 1
            bits[p2 + 3] = n >> 0 & 1
        end
    end
    return {
        bits = bits,
        pos = 1,
        read_bool = stream_read_bool,
        read_int = stream_read_int,
        read_literal = stream_read_literal,
        parse = stream_parse,
    }
end

local M = {}

function M.run(part: integer) : integer
    local stream = parse_input()
    local packet = stream:parse()
    if part == 1 then
        return packet:version_sum()
    else
        return packet:value()
    end
end

return M
