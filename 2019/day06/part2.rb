require "../util.rb"
require "./common.rb"

def path(orbits, top)
    r, pos = [], top
    top = orbits[top]
    while orbits[top]
        r << top
        top = orbits[top]
    end
    r
end

orbits = get_orbits
path1, path2 = path(orbits, 'YOU'), path(orbits, 'SAN')

path1.each do |x|
    if path2.include?(x)
        puts path1.index(x) + path2.index(x)
        break
    end
end
