require '../util.rb'

data = input_digits

image = Array.new(25 * 6) { 2 }

data.each_slice(25 * 6) do |l|
    l.each_with_index do |v, i|
        image[i] = v if image[i] == 2
    end
end

image.map { |x| x == 1 ? '#' : ' ' }.each_slice(25) do |l|
    puts l.join
end
