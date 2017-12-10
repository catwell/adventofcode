local CTX = {
    in_garbage = false,
    skip_next = false,
    depth = 0,
    score = 0,
}

local function consume_garbage(c)
    if c == "!" then
        CTX.skip_next = true
    elseif c == ">" then
        CTX.in_garbage = false
    end
end

local function consume(c)
    if CTX.skip_next then
        CTX.skip_next = false
        return
    end
    if CTX.in_garbage then
        return consume_garbage(c)
    end
    if c == "<" then
        CTX.in_garbage = true
    elseif c == "{" then
        CTX.depth = CTX.depth + 1
    elseif c == "}" then
        CTX.score = CTX.score + CTX.depth
        CTX.depth = CTX.depth - 1
    end
end

local f = assert(io.open(arg[1], "rb"))
while true do
    local c = f:read(1)
    if not c then break end
    consume(c)
end
f:close()

print(CTX.score)
