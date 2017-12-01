local grid = function()
    local g = {}
    for i=0,999 do
        g[i] = {}
        for j=0,999 do
            g[i][j] = 0
        end
    end
    return g
end

local count = function(g)
    local c = 0
    for i=0,999 do
        for j=0,999 do
            c = c + g[i][j]
        end
    end
    return c
end

local switch = function(g, a, b, c, d, f)
    for i=a,c do
        for j=b,d do
            g[i][j] = f(g[i][j])
        end
    end
end

local interp = function(g, _t, _f, _s)
    return function(s)
        local a, b, c, d = s:match("(%d+),(%d+).-(%d+),(%d+)")
        a, b, c, d = tonumber(a), tonumber(b), tonumber(c), tonumber(d)
        if s:find("^turn on") then
            switch(g, a, b, c, d, _t)
        elseif s:find("^turn off") then
            switch(g, a, b, c, d, _f)
        elseif s:find("^toggle") then
            switch(g, a, b, c, d, _s)
        end
    end
end

local g1, g2 = grid(), grid()

local f = assert(io.open(arg[1]))
local interp1 = interp(
    g1,
    function(x) return 1 end,
    function(x) return 0 end,
    function(x) return 1-x end
)
local interp2 = interp(
    g2,
    function(x) return x+1 end,
    function(x) return (x==0) and 0 or x-1 end,
    function(x) return x+2 end
)
for line in f:lines() do
    interp1(line)
    interp2(line)
end
f:close()

print(count(g1))
print(count(g2))
