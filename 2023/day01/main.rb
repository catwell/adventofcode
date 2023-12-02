nums = %w(zero one two three four five six seven eight nine)
rxp = (nums + (0..9).to_a).join("|")
fwd = Regexp.new(rxp)
bwd = Regexp.new(rxp.reverse)
rp1 = Regexp.new((0..9).to_a.join("|"))

v1 = File.new("input.txt", "r").readlines.map do |line|
    f = rp1.match(line).to_s.to_i
    b = rp1.match(line.reverse).to_s.to_i
    10 * f + b
end.sum

v2 = File.new("input.txt", "r").readlines.map do |line|
    fm = fwd.match(line).to_s
    f = nums.index(fm) || fm.to_i

    bm = bwd.match(line.reverse).to_s
    b = nums.index(bm.reverse) || bm.to_i

    10 * f + b
end.sum

puts "part 1: #{v1}"
puts "part 2: #{v2}"
