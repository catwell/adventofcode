require "../util.rb"
require "./common.rb"

def run(state, needle)
    (0..99).each do |noun|
        (0..99).each do |verb|
            tape = state.dup
            tape[1], tape[2] = noun, verb
            intcode_run(tape)
            if tape[0] == needle
                puts(100 * noun + verb)
                return
            end
        end
    end
end

run(input_integers_csv, 19690720)
