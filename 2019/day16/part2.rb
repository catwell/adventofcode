require '../util.rb'

ins = input_digits
off = 10000 * ins.length - ins[0..6].join.to_i
data = (ins.reverse * (off / ins.length + 1))[0..off - 1]

100.times do
    data.each_with_index do |_, i|
        data[i] = ((i > 0 ? data[i - 1] : 0) + data[i]) % 10
    end
end

puts data[-8..-1].reverse.join
