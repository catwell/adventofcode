def intcode_run_one(tape, pos = 0)
    i, a, b, c = tape[pos], tape[pos + 1], tape[pos + 2], tape[pos + 3]
    op, ma, mb = i % 100, i / 100 % 10, i / 1000 % 10
    va = (a && ma == 0) ? tape[a] : a
    vb = (b && mb == 0) ? tape[b] : b
    param_count, did_io = 4, nil

    case op
    when 99
        did_io = :q
        yield :q
        return
    when 1  # add
        tape[c] = va + vb
    when 2  # multiply
        tape[c] = va * vb
    when 3  # input
        param_count, did_io = 2, :i
        tape[a] = yield :i
    when 4  # output
        param_count, did_io = 2, :o
        yield :o, va
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

    [pos + param_count, did_io]
end

def intcode_run(tape, pos = 0, &on_io)
    while pos
        pos, = intcode_run_one(tape, pos, &on_io)
    end
end

def intcode_run_until_io(tape, pos = 0, &on_io)
    did_io = false
    while pos && !did_io
        pos, did_io = intcode_run_one(tape, pos, &on_io)
    end
    [pos, did_io]
end
