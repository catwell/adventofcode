require 'set'

class Intcode
    attr_accessor :tape, :pos, :relative_base

    def initialize(tape, pos: 0, relative_base: 0)
        @tape = tape
        @pos = pos
        @relative_base = relative_base
    end

    def tape_at(pos)
        tape[pos] || 0
    end

    def value(v, mode)
        case mode
        when 0
            tape_at(v)
        when 1
            v
        when 2
            tape_at(v + relative_base)
        else
            raise "unknown mode #{mode}"
        end
    end

    def tape_set(where, v, mode)
        if mode == 0
            tape[where] = v
        else
            raise unless mode == 2
            tape[where + relative_base] = v
        end
    end

    def run_one
        i = tape_at(pos)
        a, b, c = tape_at(pos + 1), tape_at(pos + 2), tape_at(pos + 3)
        op, ma, mb, mc = i % 100, i / 100 % 10, i / 1000 % 10, i / 10000 % 10
        param_count, did_io = 4, nil

        va = value(a, ma)
        vb = value(b, mb)

        case op
        when 99
            yield :q
            return [pos, :q]
        when 1  # add
            tape_set(c, va + vb, mc)
        when 2  # multiply
            tape_set(c, va * vb, mc)
        when 3  # input
            param_count, did_io = 2, :i
            v = yield :i
            tape_set(a, v, ma)
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
            tape_set(c, va < vb ? 1 : 0, mc)
        when 8  # equals
            tape_set(c, va == vb ? 1 : 0, mc)
        when 9  # adjust relative base
            @relative_base += va
            param_count = 2
        else
            raise "unknown opcode #{op} at #{pos}"
        end

        @pos += param_count

        [pos, did_io]
    end

    def run_until_io(&on_io)
        _, did_io = run_one(&on_io) until did_io
        [pos, did_io]
    end

    def run(&on_io)
        _, did_io = run_one(&on_io) until did_io == :q
    end
end

class Robot
    attr_accessor :black, :white

    def initialize
        @x, @y, @direction = 0, 0, :u
        @black, @white = Set.new, Set.new
    end

    def color
        @white.include?([@x, @y]) ? 1 : 0
    end

    def paint(v)
        if v == 0
            @white.delete([@x, @y])
            @black.add([@x, @y])
        else
            @black.delete([@x, @y])
            @white.add([@x, @y])
        end
    end

    def new_direction(v)
        case @direction
        when :u
            v == 0 ? :l : :r
        when :r
            v == 0 ? :u : :d
        when :d
            v == 0 ? :r : :l
        when :l
            v == 0 ? :d : :u
        end
    end

    def turn(v)
        @direction = new_direction(v)
    end

    def forward
        case @direction
        when :u
            @y -= 1
        when :r
            @x += 1
        when :d
            @y += 1
        when :l
            @x -= 1
        end
    end
end

def run(robot, machine)
    loop do
        machine.run_until_io do |op|
            return nil if op == :q
            raise unless op == :i
            robot.color
        end
        machine.run_until_io do |op, v|
            raise unless op == :o
            robot.paint(v)
        end
        machine.run_until_io do |op, v|
            raise unless op == :o
            robot.turn(v)
        end
        robot.forward
    end
end
