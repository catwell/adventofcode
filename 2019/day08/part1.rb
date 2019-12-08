require '../util.rb'

data = input_digits

s, lowest_count = nil, 25 * 6
data.each_slice(25 * 6) do |l|
    count = l.count(0)
    if count < lowest_count
        s, lowest_count = l, count
    end
end

puts s.count(1) * s.count(2)
