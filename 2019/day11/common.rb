require 'set'
require '../intcode.rb'

class Robot
    attr_accessor :black, :white

    def initialize
        @x, @y, @direction = 0, 0, :u
        @black, @white = Set.new, Set.new
    end

    def color
        @white.include?([@x, @y]) ? 1 : 0
    end

    def paint(v)
        if v == 0
            @white.delete([@x, @y])
            @black.add([@x, @y])
        else
            @black.delete([@x, @y])
            @white.add([@x, @y])
        end
    end

    def new_direction(v)
        case @direction
        when :u
            v == 0 ? :l : :r
        when :r
            v == 0 ? :u : :d
        when :d
            v == 0 ? :r : :l
        when :l
            v == 0 ? :d : :u
        end
    end

    def turn(v)
        @direction = new_direction(v)
    end

    def forward
        case @direction
        when :u
            @y -= 1
        when :r
            @x += 1
        when :d
            @y += 1
        when :l
            @x -= 1
        end
    end
end

def run(robot, machine)
    loop do
        machine.run_until_io do |op|
            return nil if op == :q
            raise unless op == :i
            robot.color
        end
        machine.run_until_io do |op, v|
            raise unless op == :o
            robot.paint(v)
        end
        machine.run_until_io do |op, v|
            raise unless op == :o
            robot.turn(v)
        end
        robot.forward
    end
end
