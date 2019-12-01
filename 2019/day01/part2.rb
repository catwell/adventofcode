require "../util.rb"

def fuel(x)
    r = x/3 - 2
    r > 0  ? r + fuel(r) : 0
end

puts input_integers.map{ |x| fuel(x) }.reduce(:+)
