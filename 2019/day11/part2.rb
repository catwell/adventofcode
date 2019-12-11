require '../util.rb'
require './common.rb'

state = input_integers_csv

robot = Robot.new
machine = Intcode.new(state)

robot.white << [0, 0]
run(robot, machine)

white = robot.white

grid = Array.new(6) { Array.new(40) { ' ' } }
white.each { |x, y| grid[y][x] = '#' }
puts grid.map(&:join).join("\n")
