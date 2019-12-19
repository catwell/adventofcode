require '../util.rb'
require './common.rb'

tape = input_integers_csv

count = 0
0.upto(49) do |y|
    0.upto(49) do |x|
        count += 1 if beam_at?(tape, x, y)
    end
end

puts count
