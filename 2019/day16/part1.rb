require '../util.rb'
require './common.rb'

puts fft(input_digits, 100)[0..7].map(&:to_s).join
