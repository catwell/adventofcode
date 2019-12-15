require '../util.rb'
require './common.rb'

tape = input_integers_csv

state = State.new(tape)
oxygen = state.step until oxygen
puts state.steps
