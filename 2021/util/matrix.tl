local type Matrix = record<A>
    data: {{A}}
    default: A
    max_row: integer
    max_col: integer
    get: function(Matrix<A>, integer, integer): A
    set: function(Matrix<A>, integer, integer, A)
end

local function get<A>(self: Matrix<A>, row: integer, col: integer): A
    if self.data[row] == nil then
        return self.default
    end
    if self.data[row][col] == nil then
        return self.default
    end
    return self.data[row][col]
end

local function set<A>(self: Matrix<A>, row: integer, col: integer, v: A)
    if self.data[row] == nil then
        self.data[row] = {}
    end
    if row > self.max_row then self.max_row = row end
    if col > self.max_col then self.max_col = col end
    self.data[row][col] = v
end

local mt = {__index = {get = get, set = set}}

local function new<A>(default: A): Matrix<A>
    local r : Matrix<A> = {data = {}, default = default, max_row = 0, max_col = 0}
    return setmetatable(r, mt)
end

local M = {new = new, Matrix = Matrix}

return M
