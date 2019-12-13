require '../util.rb'
require './common.rb'

state = input_integers_csv

def run(machine)
    loop do
        x, y = nil, nil
        machine.run_until_io do |op, v|
            return nil if op == :q
            raise unless op == :o
            x = v
        end
        machine.run_until_io do |op, v|
            raise unless op == :o
            y = v
        end
        machine.run_until_io do |op, v|
            raise unless op == :o
            yield x, y, v
        end
    end
end

machine = Intcode.new(state.dup)
plays = []
run(machine) do |x, y, v|
    plays << [x, y, v]
end
x_max = plays.map { |x| x[0] }.max
y_max = plays.map { |y| y[0] }.max

machine = Intcode.new(state.dup)
grid = Array.new(y_max) { Array.new(x_max) { 0 } }
run(machine) do |x, y, v|
    grid[y][x] = v
end

puts(grid.flatten.count { |x| x == 2 })
