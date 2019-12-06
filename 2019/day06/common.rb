def get_orbits
    orbits = {}
    input_file.each do |l|
        orbited, orbiter = l.strip.split(')')
        orbits[orbiter] = orbited
    end
    orbits
end
