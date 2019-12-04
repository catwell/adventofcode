require "./common.rb"

puts(run do |digits|
    digits.each_with_index.find { |v, i| v == digits[i+1] }
end)
