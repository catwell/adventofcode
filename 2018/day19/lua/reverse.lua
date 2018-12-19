-- This is my input reversed as a Lua program.

local r1, r3
local r2, r5 = 0, 0
local r0 = 0 -- Set to 0 for part 1, 1 for part 2 (very slow...).

goto L17

::L01::
r5 = math.floor(r5) -- for performance and display only
r3 = 1

::L02::
r1 = 1

::L03::

if r3 * r1 == r5 then
    -- R5 is a multiple of R3
    r0 = r0 + r3
end
r1 = r1 + 1
if r1 <= r5 then
    goto L03
end

-- ::L12::
r3 = r3 + 1
if r3 <= r5 then goto L02 end
do
    print(r0)
    return
end

::L17::
r5 = 19 * 11 * (r5 + 2) ^ 2
r2 = 22 * (r2 + 5) + 18
r5 = r5 + r2

if r0 == 0 then goto L01 end

-- ::L27::
r2 = (27 * 28 + 29) * 30 * 14 * 32
r5 = r5 + r2
r0 = 0
goto L01
