def input_integers
    File.new("input.txt", "r").each.map(&:to_i)
end

def input_integers_csv
    File.new("input.txt", "r").read.chomp.split(",").map(&:to_i)
end
