--[[

ILP Solver in Teal
Minimize sum(x) subject to Ax = b, x >= 0, integers

Integrally written by Gemini. I know, I know.
But I don't know a good library for this in Lua
and I don't have time to do this by hand.

Usage for first machine in example i.e.
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}

local A = {
    { 0, 0, 0, 0, 1, 1 },
    { 0, 1, 0, 0, 0, 1 },
    { 0, 0, 1, 1, 1, 0 },
    { 1, 1, 0, 1, 0, 0 },
}
local b = {3, 5, 4, 7}

local solver = ILPSolver.new(A, b)
local sol, cost = solver:solve()

print("Minimum Sum: ", cost)
print("Variables: ", table.concat(sol, ","))

]]

local type Vector = {number}
local type Matrix = {Vector}

local record ILPSolver
    A: Matrix
    b: Vector
    M: integer
    N: integer

    -- Solution state
    best_solution: Vector | nil
    min_total: number

    -- Internal solver state
    pivots: {integer:integer}   -- map<col_index, row_index>
    free_vars: {integer}        -- list<col_index>
    free_weights: {integer:number} -- map<col_index, weight>

    -- Constants
    epsilon: number
    search_limit: integer
    base_cost: number

    -- Constructor
    new: function(A: Matrix, b: Vector): ILPSolver

    -- Methods
    is_int: function(self: ILPSolver, n: number): boolean
    round: function(self: ILPSolver, n: number): integer
    perform_rref: function(self: ILPSolver)
    calculate_weights: function(self: ILPSolver)
    search: function(self: ILPSolver, free_idx_ptr: integer, current_assign: {integer:integer})
    solve: function(self: ILPSolver): Vector | nil, number
end

local ILPSolverMetatable: metatable<ILPSolver> = {
    __index = ILPSolver
}

function ILPSolver.new(A: Matrix, b: Vector): ILPSolver
    local self = setmetatable({} as ILPSolver, ILPSolverMetatable)

    -- Deep copy input to preserve original data
    self.M = #A
    self.N = #A[1]
    self.A = {}
    self.b = {}

    for i = 1, #b do
        table.insert(self.b, b[i])
    end

    for i = 1, self.M do
        local row: Vector = {}
        for j = 1, self.N do
            table.insert(row, A[i][j])
        end
        table.insert(self.A, row)
    end

    self.best_solution = nil
    self.min_total = math.huge
    self.epsilon = 1e-9
    self.search_limit = 20
    self.pivots = {}
    self.free_vars = {}
    self.free_weights = {}
    self.base_cost = 0

    return self
end

function ILPSolver:is_int(n: number): boolean
    return math.abs(n - math.floor(n + 0.5)) < self.epsilon
end

function ILPSolver:round(n: number): integer
    return math.floor(n + 0.5)
end

-- Gaussian Elimination to Reduced Row Echelon Form
function ILPSolver:perform_rref()
    local pivot_row = 1
    self.pivots = {}
    self.free_vars = {}

    for col = 1, self.N do
        if pivot_row <= self.M then
            -- Find pivot
            local swap = pivot_row
            while swap <= self.M and math.abs(self.A[swap][col]) < self.epsilon do
                swap = swap + 1
            end

            if swap <= self.M then
                -- Swap rows
                self.A[pivot_row], self.A[swap] = self.A[swap], self.A[pivot_row]
                local temp_b = self.b[pivot_row]
                self.b[pivot_row] = self.b[swap]
                self.b[swap] = temp_b

                -- Normalize
                local div = self.A[pivot_row][col]
                for j = col, self.N do
                    self.A[pivot_row][j] = self.A[pivot_row][j] / div
                end
                self.b[pivot_row] = self.b[pivot_row] / div

                -- Eliminate
                for i = 1, self.M do
                    if i ~= pivot_row then
                        local factor = self.A[i][col]
                        for j = col, self.N do
                            self.A[i][j] = self.A[i][j] - (factor * self.A[pivot_row][j])
                        end
                        self.b[i] = self.b[i] - (factor * self.b[pivot_row])
                    end
                end

                self.pivots[col] = pivot_row
                pivot_row = pivot_row + 1
            else
                table.insert(self.free_vars, col)
            end
        else
            table.insert(self.free_vars, col)
        end
    end
end

-- Calculate objective weights for free variables
function ILPSolver:calculate_weights()
    self.base_cost = 0
    self.free_weights = {}

    -- Initial weight 1.0 for all free variables
    for _, f_idx in ipairs(self.free_vars) do
        self.free_weights[f_idx] = 1.0
    end

    for col = 1, self.N do
        local row = self.pivots[col]
        if row then
            -- Add constant part from pivot equation to base cost
            self.base_cost = self.base_cost + self.b[row]

            -- Adjust weights based on coefficients (pivot = b - coeff*free)
            for _, f_idx in ipairs(self.free_vars) do
                local coeff = self.A[row][f_idx]
                local current_w = self.free_weights[f_idx] or 0.0
                self.free_weights[f_idx] = current_w - coeff
            end
        end
    end
end

function ILPSolver:search(free_idx_ptr: integer, current_assign: {integer:integer})
    -- Base Case: All free variables assigned
    if free_idx_ptr > #self.free_vars then
        local solution: Vector = {}
        local valid = true

        -- Initialize solution vector
        for i = 1, self.N do solution[i] = 0 end

        -- Assign Free Variables
        for _, f_idx in ipairs(self.free_vars) do
            solution[f_idx] = current_assign[f_idx]
        end

        -- Compute Pivot Variables and check constraints
        for col = 1, self.N do
            local row = self.pivots[col]
            if row then
                local val = self.b[row]
                for _, f_idx in ipairs(self.free_vars) do
                    val = val - (self.A[row][f_idx] * solution[f_idx])
                end

                -- Constraints: Integer and Non-Negative
                if val < -self.epsilon or not self:is_int(val) then
                    valid = false
                    break
                end
                solution[col] = self:round(val)
            end
        end

        if valid then
            local total = 0.0
            for i = 1, self.N do total = total + solution[i] end

            if total < self.min_total then
                self.min_total = total
                -- Create a clean copy for storage
                local stored_sol: Vector = {}
                for i = 1, self.N do table.insert(stored_sol, solution[i]) end
                self.best_solution = stored_sol
            end
        end
        return
    end

    -- Recursive Step
    local f_var = self.free_vars[free_idx_ptr]

    -- Search range [0..limit]
    for val = 0, self.search_limit do
        current_assign[f_var] = val
        self:search(free_idx_ptr + 1, current_assign)
    end

    current_assign[f_var] = nil -- cleanup
end

function ILPSolver:solve(): Vector | nil, number
    self:perform_rref()
    self:calculate_weights()

    local initial_assign: {integer:integer} = {}
    self:search(1, initial_assign)

    return self.best_solution, self.min_total
end

return ILPSolver
