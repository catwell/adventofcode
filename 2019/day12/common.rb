class Moon
    attr_accessor :pos, :vel

    def initialize(x, y, z)
        @pos = [x, y, z]
        @vel = [0, 0, 0]
    end

    def move
        (0..2).each do |i|
            @pos[i] += @vel[i]
        end
    end

    def energy
        @pos.map(&:abs).reduce(:+) * @vel.map(&:abs).reduce(:+)
    end

    def to_s
        "pos=<x= #{@pos[0]}, y=#{@pos[1]}, z= #{@pos[2]}>, " \
        "vel=<x= #{@vel[0]}, y=#{@vel[1]}, z= #{@vel[2]}>"
    end
end

INITIAL_STATE = [
    Moon.new(19, -10, 7),
    Moon.new(1, 2, -3),
    Moon.new(14, -4, 1),
    Moon.new(8, 7, -6)
].freeze
