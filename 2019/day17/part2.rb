require '../util.rb'
require '../intcode.rb'

tape = input_integers_csv
tape[0] = 2
machine = Intcode.new(tape)

a = 'L,10,R,10,L,10,L,10'
b = 'R,12,L,12,R,6'
c = 'R,10,R,12,L,12'
main = 'A,C,A,C,B,B,C,A,C,B'

code = [main, a, b, c, 'n', ''].join("\n").chars.map(&:ord)

machine.run do |op, v|
    if op == :i
        code.shift
    else
        puts v if op == :o && v > 255
    end
end
