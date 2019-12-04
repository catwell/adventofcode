require "./common.rb"

puts lookup { |pt| pt[:x].abs + pt[:y].abs }
