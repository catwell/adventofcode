local util = require "util"

local type Ticket = {number}

local record Range
    min: number
    max: number
end

local type Ranges = {string:{Range}}

local record State
    ranges: Ranges
    my_ticket: Ticket
    tickets: {Ticket}
    check_number_validity: function(State, number): {string}
    check_ticket_validity: function(State, Ticket): boolean, number
    scanning_error_rate: function(State): number
    discard_invalid_tickets: function(State)
    determine_columns: function(State): {string:number}
    decoded_ticket: function(State): {string:number}
end

local M = {}

local function state_check_number_validity(self: State, n: number): {string}
    local r = {}
    for field, ranges in pairs(self.ranges) do
        for _, range in ipairs(ranges) do
            if n >= range.min and n <= range.max then
                table.insert(r, field)
            end
        end
    end
    return r
end

local function state_check_ticket_validity(self: State, ticket: Ticket): boolean, number
    for _, n in ipairs(ticket) do
        if #self:check_number_validity(n) == 0 then
            return false, n
        end
    end
    return true, 0
end

local function state_scanning_error_rate(self: State): number
    local r = 0
    for _, ticket in ipairs(self.tickets) do
        local _, n = self:check_ticket_validity(ticket)
        r = r + n
    end
    return r
end

local function state_discard_invalid_tickets(self: State)
    local tickets: {Ticket} = {}
    for _, ticket in ipairs(self.tickets) do
        if self:check_ticket_validity(ticket) then
            table.insert(tickets, ticket)
        end
    end
    self.tickets = tickets
end

local function _columns_for(state: State, ranges: {Range}): {number:boolean}
    local r = {}
    for i = 1, #state.my_ticket do
        local possible = true
        for _, ticket in ipairs(state.tickets) do
            possible = false
            for _, range in ipairs(ranges) do
                if ticket[i] >= range.min and ticket[i] <= range.max then
                    possible = true
                end
            end
            if not possible then break end
        end
        if possible then r[i] = true end
    end
    return r
end

local function _cleanup(possible_columns: {string:{number:boolean}}, field: string, column: number)
    for cur_field, possibilities in pairs(possible_columns) do
        if cur_field ~= field then
            possibilities[column] = nil
        end
    end
end

local function state_determine_columns(self: State) : {string:number}
    local possible_columns : {string:{number: boolean}} = {}
    for field, ranges in pairs(self.ranges) do
        possible_columns[field] = _columns_for(self, ranges)
    end
    local found = true
    while found do
        found = false
        for field, possibilities in pairs(possible_columns) do
            if util.count_keys(possibilities as {any:any}) == 1 then
                _cleanup(possible_columns, field, (next(possibilities)))
            else
                found = true
            end
        end
    end
    local r = {}
    for field, possibilities in pairs(possible_columns) do
        r[field] = next(possibilities)
    end
    return r
end

local function state_decoded_ticket(self: State) : {string:number}
    local columns = self:determine_columns()
    local r = {}
    for field, i in pairs(columns) do
        r[field] = self.my_ticket[i]
    end
    return r
end

local state_mt = {__index = {
    check_number_validity = state_check_number_validity,
    check_ticket_validity = state_check_ticket_validity,
    scanning_error_rate = state_scanning_error_rate,
    discard_invalid_tickets = state_discard_invalid_tickets,
    determine_columns = state_determine_columns,
    decoded_ticket = state_decoded_ticket,
}}

local function new_state(initial: {number}): State
    return setmetatable({}, state_mt)
end

local function parse_ranges_line(s: string) : string, {Range}
    local ptrn = "^([^:]+): (%d+)-(%d+) or (%d+)-(%d+)$"
    local field, min1, max1, min2, max2 = s:match(ptrn)
    return field, {
        {min = tonumber(min1), max = tonumber(max1)},
        {min = tonumber(min2), max = tonumber(max2)},
    }
end

local function parse_ranges(s: string): Ranges
    local lines = util.split(s, "\n")
    local r : Ranges = {}
    for _, v in ipairs(lines) do
        local field, ranges = parse_ranges_line(v)
        r[field] = ranges
    end
    return r
end

local function parse_tickets(s: string): {Ticket}
    local lines = util.split(s, "\n")
    table.remove(lines, 1)
    local function f(s2: string) : Ticket
        return util.parse_integers(s2)
    end
    return util.map(lines, f)
end

function M.parse_input() : State
    local input = util.split(util.read_file("input.txt"), "\n\n")
    local r = new_state()
    r.ranges = parse_ranges(input[1])
    r.my_ticket = parse_tickets(input[2])[1]
    r.tickets = parse_tickets(input[3])
    return r
end

return M
