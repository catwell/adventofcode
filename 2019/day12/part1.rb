require './common.rb'

state = INITIAL_STATE.dup

def step(state)
    state.combination(2).each do |m1, m2|
        (0..2).each do |i|
            if m1.pos[i] > m2.pos[i]
                m1.vel[i] -= 1
                m2.vel[i] += 1
            elsif m2.pos[i] > m1.pos[i]
                m2.vel[i] -= 1
                m1.vel[i] += 1
            end
        end
    end
    state.each(&:move)
end

1000.times { step(state) }
puts state.map(&:energy).reduce(&:+)
