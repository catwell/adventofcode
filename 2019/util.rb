def input_integers
    File.new("input.txt", "r").each.map(&:to_i)
end
