require '../util.rb'
require './common.rb'

tape = input_integers_csv

def ratio(y)
    (4100 * y) / 5000 # found visually
end

def x_borders(tape, y, x0 = nil)
    x0 ||= ratio(y)
    raise unless beam_at?(tape, x0, y)
    xl, xr = x0, x0
    xl -= 1 while beam_at?(tape, xl, y)
    xr += 1 while beam_at?(tape, xr, y)
    [xl + 1, xr - 1]
end

def fit_at(tape, y)
    b0 = x_borders(tape, y)
    b1 = x_borders(tape, y + 99)
    b0[1] - b1[0] + 1
end

1000.upto(2000) do |y|
    if fit_at(tape, y) == 100
        x = x_borders(tape, y)[1] - 99
        puts 10000 * x + y
        break
    end
end
