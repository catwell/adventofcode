def intcode_run(tape, pos = 0)
    op, a, b, c = tape[pos], tape[pos + 1], tape[pos + 2], tape[pos + 3]
    if op == 99
        return
    elsif op == 1
        tape[c] = tape[a] + tape[b]
    elsif tape[pos] == 2
        tape[c] = tape[a] * tape[b]
    else
        raise "unknown opcode #{op} at #{pos}"
    end
    intcode_run(tape, pos + 4)
end
