require "../util.rb"
require "./common.rb"

def branch_length(orbits, s)
    orbits[s] ? (branch_length(orbits, orbits[s]) + 1) : 0
end

orbits = get_orbits
puts orbits.keys.map { |x| branch_length(orbits, x) }.reduce(&:+)
