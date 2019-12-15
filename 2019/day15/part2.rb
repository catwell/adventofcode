require '../util.rb'
require './common.rb'

tape = input_integers_csv

state = State.new(tape)
oxygen = state.step until oxygen

state = State.new(tape)
state.frontier = [oxygen]
state.map = { oxygen.pos => 2 }
state.step until state.frontier.empty?
puts state.steps - 1
