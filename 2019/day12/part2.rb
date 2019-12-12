require './common.rb'
require 'set'

def step_1d(moons)
    moons.combination(2).each do |m1, m2|
        if m1[0] > m2[0]
            m1[1] -= 1
            m2[1] += 1
        elsif m2[0] > m1[0]
            m2[1] -= 1
            m1[1] += 1
        end
    end
    moons.each { |m| m[0] += m[1] }
end

def find_1d_period(moons)
    observed, k = [], 0
    loop do
        if moons.map { |x| x[1].abs }.reduce(&:+) == 0
            repr = moons.to_s
            return k if observed.include?(repr)
            observed << repr
        end
        k += 1
        step_1d(moons)
    end
end

state = INITIAL_STATE

puts((0..2).map do |i|
    m1d = state.map { |moon| [moon.pos[i], 0] }
    find_1d_period(m1d)
end.reduce(:lcm))
