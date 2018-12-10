local M = {}

function M.parse_input(s)
    local ptrn = "(%d+) players; last marble is worth (%d+) points"
    local c, n = string.match(s, ptrn)
    return tonumber(c), tonumber(n)
end

local MM = {}

function M.new_game(players_count, last_marble)
    local self = {
        player = 1,
        players_count = players_count,
        last_marble = last_marble,
    }

    local cell = {v = 0}
    cell.n, cell.p = cell, cell
    self.cell = cell

    local scores = {}
    for i = 1, players_count do scores[i] = 0 end
    self.scores = scores

    return setmetatable(self, {__index = MM})
end

function MM.next_player(self)
    local p = (self.player + 1) % self.players_count
    self.player = (p == 0) and self.players_count or p
end

function MM.high_score(self)
    local r = 0
    for i = 1, self.players_count do
        if self.scores[i] > r then
            r = self.scores[i]
        end
    end
    return r
end

function MM.play_normal(self, v)
    local p = self.cell.n
    local n = p.n
    local c = {v = v, p = p, n = n}
    p.n, n.p, self.cell = c, c, c
    self:next_player()
end

function MM.play_special(self, v)
    for _ = 1, 6 do
        self.cell = self.cell.p
    end
    self.scores[self.player] = self.scores[self.player] + v + self.cell.p.v
    self.cell.p = self.cell.p.p
    self.cell.p.n = self.cell
    self:next_player()
end

function MM.play(self)
    for i = 1, self.last_marble do
        if i % 23 == 0 then
            self:play_special(i)
        else
            self:play_normal(i)
        end
    end
end

return M
