require '../intcode.rb'

DIR = { N: 1, S: 2, W: 3, E: 4 }.freeze
DELTA = { N: [0, 1], E: [1, 0], W: [-1, 0], S: [0, -1] }.freeze

class P
    attr_accessor :machine, :path, :x, :y, :floor

    def initialize(machine, path = [], x = 0, y = 0, floor = 1)
        @machine, @path = machine, path
        @x, @y = x, y
        @floor = floor
    end

    def pos
        [x, y]
    end

    def move(dir)
        delta = DELTA[dir]
        new_machine, floor = machine.dup, nil
        new_machine.run_until_io { DIR[dir] }
        new_machine.run_until_io { |_, v| floor = v }
        P.new(
            new_machine, path.dup + [dir],
            x + delta[0], y + delta[1], floor
        )
    end
end

class State
    attr_accessor :map, :frontier, :steps

    def initialize(tape, map = nil)
        @map = map || { [0, 0] => 1 }
        @frontier = [P.new(Intcode.new(tape))]
        @steps = 0
    end

    def step
        @steps += 1
        new_frontier = []
        frontier.each do |p|
            DELTA.each do |dir, v|
                next if map[[p.x + v[0], p.y + v[1]]]
                new_p = p.move(dir)
                map[new_p.pos] = new_p.floor
                if new_p.floor == 2
                    return new_p
                elsif new_p.floor == 1
                    new_frontier << new_p
                end
            end
        end
        @frontier = new_frontier.uniq(&:pos)
        nil
    end
end
