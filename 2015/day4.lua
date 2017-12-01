-- much faster than Bash :)

local md5 = (require "md5").sumhexa
local key = arg[1]

local n = 1
while true do
    if md5(key .. tostring(n)):sub(1,5) == "00000" then break end
    n = n + 1
end
print(n)
while true do
    if md5(key .. tostring(n)):sub(1,6) == "000000" then break end
    n = n + 1
end
print(n)
