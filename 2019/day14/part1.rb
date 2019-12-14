require '../util.rb'
require './common.rb'

reactions = parse
state = State.new(reactions)
state.needs['FUEL'] = 1
state.fulfill_needs

puts state.ore
