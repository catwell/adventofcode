-- So, I reversed the ASM (see analysis.txt and reverse.lua).
-- What it does is compute R5, then sum all its divisors.
-- In part 2, R5 is 10551364 so its prime factors are 2, 2, 37 and 71293.

print(
    1 +
    2 +
    37 +
    71293 +
    2 * 2 +
    2 * 37 +
    2 * 71293 +
    37 * 71293 +
    2 * 2 * 37 +
    2 * 2 * 71293 +
    2 * 37 * 71293 +
    2 * 2 * 37 * 71293
)
