require '../util.rb'
require './common.rb'

state = input_integers_csv

machine = Intcode.new(state.dup)
machine.tape[0] = 2

def run(machine)
    score, pad_x, ball_x = nil, nil, nil
    loop do
        x, y, prev_i = nil, nil, false
        machine.run_until_io do |op, v|
            return score if op == :q
            if op == :i
                prev_i = true
                if pad_x == ball_x
                    0
                elsif pad_x < ball_x
                    1
                else
                    -1
                end
            else
                raise unless op == :o
                x = v
            end
        end
        next if prev_i
        machine.run_until_io do |op, v|
            raise unless op == :o
            y = v
        end
        machine.run_until_io do |op, v|
            raise unless op == :o
            if x == -1 && y == 0
                score = v
            elsif v == 3
                pad_x = x
            elsif v == 4
                ball_x = x
            end
        end
    end
end

puts run(machine)
