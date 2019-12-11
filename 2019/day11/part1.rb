require '../util.rb'
require './common.rb'

state = input_integers_csv

robot = Robot.new
machine = Intcode.new(state)

run(robot, machine)
puts robot.black.count + robot.white.count
