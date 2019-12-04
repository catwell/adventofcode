def run(&has_neighbors)
    digits = [1, 2, 8, 8, 8, 8]
    max_pos = digits.length - 1
    pos = max_pos
    count = has_neighbors.call(digits) ? 1 : 0
    loop do
        if digits[pos] < 9
            digits[pos] += 1
            (pos + 1).upto(max_pos) { |i| digits[i] = digits[pos] }
            return count if digits[0] == 6
            count += 1 if has_neighbors.call(digits)
            pos = max_pos
        else
            pos -= 1
        end
    end
end
