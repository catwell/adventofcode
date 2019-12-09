require '../util.rb'
require './common.rb'

tape = input_integers_csv

def output_for(tape, settings)
    machines = settings.map do |phase|
        machine = Intcode.new(tape.dup)
        machine.run_until_io do |op|
            raise unless op == :i
            phase
        end
        machine
    end
    v, running = 0, 5
    while running > 0
        machines.each do |machine|
            machine.run_until_io do |op|
                if op == :q
                    running -= 1
                else
                    raise unless op == :i
                    v
                end
            end
            machine.run_until_io do |op, x|
                raise unless op == :o
                v = x
            end
        end
    end
    v
end

max_v = 0
(5..9).to_a.permutation.each do |settings|
    max_v = [max_v, output_for(tape, settings)].max
end

puts max_v
