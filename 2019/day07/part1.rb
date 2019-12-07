require '../util.rb'
require './common.rb'

state = input_integers_csv

def output_for(state, settings)
    v = 0
    settings.each do |s|
        first = true
        intcode_run(state) do |op, x|
            if op == :i
                r = first ? s : v
                first = false
                r
            elsif op == :o
                v = x
            end
        end
    end
    v
end

max_v = 0
(0..4).to_a.permutation.each do |settings|
    max_v = [max_v, output_for(state, settings)].max
end

puts max_v
