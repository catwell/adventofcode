require '../intcode.rb'

def beam_at?(tape, x, y)
    machine = Intcode.new(tape.dup)
    machine.run_until_io { x }
    machine.run_until_io { y }
    machine.run_until_io do |_, v|
        return v == 1
    end
end
