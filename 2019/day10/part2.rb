require '../util.rb'
require './common.rb'

space = input_file.readlines.map do |l|
    l.strip.each_char.map { |c| c == '#' }
end

y_max, x_max = space.length - 1, space[0].length - 1
all_angles = angles([x_max, y_max].max)

x0, y0, count = 26, 29, 0
all_angles.each do |x, y|
    h = hits_at_angle(space, x0, y0, x, y, x_max, y_max)
    if h
        count += 1
        puts h[0] * 100 + h[1] if count == 200
    end
end
