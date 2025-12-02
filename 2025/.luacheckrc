-- Configured to check tl output.

ignore = {
    -- line is too long
    "631",
    -- ignore "unused variable" for records (starting with an uppercase letter)
    "211/[A-Z].*",
}
