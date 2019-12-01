require "../util.rb"

puts input_integers.map{|x| x/3 - 2}.reduce(:+)
