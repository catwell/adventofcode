require '../util.rb'
require './common.rb'

tape = input_integers_csv

def run_until_io(state, &on_io)
    intcode_run_until_io(state[:tape], state[:pos], &on_io)
end

def output_for(tape, settings)
    states = settings.map do |phase|
        { phase: phase, tape: tape.dup, pos: 0 }
    end
    states.each do |state|
        state[:pos], = run_until_io(state) do |op|
            raise unless op == :i
            state[:phase]
        end
    end
    v, running = 0, 5
    while running > 0
        states.each do |state|
            state[:pos], = run_until_io(state) do |op|
                if op == :q
                    running -= 1
                else
                    raise unless op == :i
                    v
                end
            end
            state[:pos], = run_until_io(state) do |op, x|
                if op == :q
                    running -= 1
                else
                    raise unless op == :o
                    v = x
                end
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
