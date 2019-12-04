def input_file
    File.new("input.txt", "r")
end

def input_integers
    input_file.each.map(&:to_i)
end

def input_integers_csv
    input_file.read.chomp.split(",").map(&:to_i)
end
