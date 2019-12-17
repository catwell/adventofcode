require '../util.rb'
require '../intcode.rb'

tape = input_integers_csv
machine = Intcode.new(tape)

map, x, y = {}, 0, 0
machine.run do |op, v|
    if op == :o
        c = v.chr
        if c == "\n"
            x, y = 0, y + 1
        else
            map[[x, y]] = c if c != '.'
            x += 1
        end
    end
end

s = 0
map.each do |coord, v|
    x, y = coord
    if (
        v == '#' &&
        map[[x - 1, y]] == '#' &&
        map[[x + 1, y]] == '#' &&
        map[[x, y - 1]] == '#' &&
        map[[x, y + 1]] == '#'
    )
        s += x * y
    end
end

puts s
