def intcode_run(tape, input, pos = 0)
    i, a, b, c = tape[pos], tape[pos + 1], tape[pos + 2], tape[pos + 3]
    op, ma, mb, mc = i % 100, i / 100 % 10, i / 1000 % 10, i / 10000 % 10
    va = (a && ma == 0) ? tape[a] : a
    vb = (b && mb == 0) ? tape[b] : b
    param_count = 4

    case op
    when 99
        return
    when 1  # add
        tape[c] = va + vb
    when 2  # multiply
        tape[c] = va * vb
    when 3  # input
        param_count = 2
        tape[a] = input
    when 4  # output
        param_count = 2
        puts va
    when 5  # jump-if-true
        if va != 0
            pos = vb
            param_count = 0
        else
            param_count = 3
        end
    when 6  # jump-if-false
        if va == 0
            pos = vb
            param_count = 0
        else
            param_count = 3
        end
    when 7  # less than
        tape[c] = va < vb ? 1 : 0
    when 8  # equals
        tape[c] = (va == vb) ? 1 : 0
    else
        raise "unknown opcode #{op} at #{pos}"
    end

    intcode_run(tape, input, pos + param_count)
end
