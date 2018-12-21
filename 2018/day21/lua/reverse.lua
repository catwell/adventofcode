local r2 = 0
local r1, r3, r5
local r0 = 0 --  Change me.

-- check bitwise and works
::L00::
if 123 & 456 ~= 72 then
    goto L00
end

::L06::
r5 = r2 | 65536
r2 = 5234604

::L08::
r3 = r5 & 255
r2 = (((r2 + r3) & 16777215) * 65899) & 16777215

if 256 > r5 then
    goto L28
end

r3 = 0

::L18::
-- This basically does `r3 = r5 // 256`.
r1 = 256 * (r3 + 1)
if r1 > r5 then
    goto L26
end
r3 = r3 + 1
goto L18

::L26::
r5 = r3
goto L08

::L28::
if r2 ~= r0 then
    goto L06
end
