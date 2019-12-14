require '../util.rb'
require './common.rb'

reactions = parse

state = State.new(reactions)
state.needs['FUEL'] = 1
state.fulfill_needs
unit_ore = state.ore
available_ore = 1000000000000
fuel_produced = 0

state = State.new(reactions)

loop do
    to_produce = (available_ore - state.ore) / unit_ore
    to_produce = 1 if to_produce < 1
    state.needs['FUEL'] = to_produce
    state.fulfill_needs
    break if state.ore > available_ore
    fuel_produced += to_produce
end

puts fuel_produced
