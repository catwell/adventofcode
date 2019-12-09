class Intcode
    attr_accessor :tape, :pos

    def initialize(tape, pos: 0)
        @tape = tape
        @pos = pos
    end

    def run_one
        i, a, b, c = tape[pos], tape[pos + 1], tape[pos + 2], tape[pos + 3]
        op, ma, mb = i % 100, i / 100 % 10, i / 1000 % 10
        param_count, did_io = 4, nil

        va = (a && ma == 0) ? tape[a] : a
        vb = (b && mb == 0) ? tape[b] : b

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
                @pos = vb
                param_count = 0
            else
                param_count = 3
            end
        when 6  # jump-if-false
            if va == 0
                @pos = vb
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

        @pos += param_count

        [pos, did_io]
    end

    def run_until_io(&on_io)
        did_io = false
        while pos && !did_io
            @pos, did_io = run_one(&on_io)
        end
        [pos, did_io]
    end

    def run(&on_io)
        while pos
            @pos, = run_one(&on_io)
        end
    end
end
