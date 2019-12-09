require '../util.rb'
require './common.rb'

state = input_integers_csv

machine = Intcode.new(state)
machine.run_until_io { 2 }
machine.run { |op, x| puts x if op == :o }
