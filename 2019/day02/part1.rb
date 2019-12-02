require "../util.rb"
require "./common.rb"

state = input_integers_csv
state[1], state[2] = 12, 2

intcode_run(state)
puts state[0]
