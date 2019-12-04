require "./common.rb"

puts(run do |digits|
    digits.each_with_index.find do |v, i|
        (v == digits[i+1]) && (v != digits[i+2]) && (
            (i == 0) || (v != digits[i-1])
        )
    end
end)
