require '../util.rb'
require './common.rb'

space = input_file.readlines.map do |l|
    l.strip.each_char.map { |c| c == '#' }
end

def hits_count(space, x0, y0, x_max, y_max, all_angles)
    all_angles.map do |x, y|
        hits_at_angle(space, x0, y0, x, y, x_max, y_max) ? 1 : 0
    end.reduce(&:+)
end

y_max, x_max = space.length - 1, space[0].length - 1
all_angles = angles([x_max, y_max].max)

all_hits_counts = space.each_with_index.map do |l, y|
    l.each_with_index.map do |a, x|
        a ? hits_count(space, x, y, x_max, y_max, all_angles) : 0
    end
end

max = all_hits_counts.flatten.max
puts max

# for part 2
all_hits_counts.each_with_index.map do |l, y|
    l.each_with_index.map do |v, x|
        puts [x, y].to_s if v == max
    end
end
