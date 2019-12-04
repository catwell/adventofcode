require "../util.rb"

def parse_move(s)
    {dir: s[0], len: s[1..-1].to_i}
end

def parse_line(s)
    s.chomp.split(",").map{|x| parse_move(x)}
end

def segments(moves)
    # Origin in the output is always the lower left point.
    pos, steps = {x: 0, y: 0}, 0
    moves.map do |move|
        case move[:dir]
        when "U"
            orig = pos.dup
            pos[:y] += move[:len]
        when "D"
            pos[:y] -= move[:len]
            orig = pos.dup
        when "L"
            pos[:x] -= move[:len]
            orig = pos.dup
        when "R"
            orig = pos.dup
            pos[:x] += move[:len]
        end
        steps += move[:len]
        {
            orig: orig,
            dir: move[:dir],
            len: move[:len],
            steps: steps - move[:len],
        }
    end
end

def vertical?(s)
    s[:dir] == "U" || s[:dir] == "D"
end

def intersect_ortho(sv, sh)
    x, y = sv[:orig][:x], sh[:orig][:y]
    if (
        x >= sh[:orig][:x] && x <= (sh[:orig][:x] + sh[:len]) &&
        y >= sv[:orig][:y] && y <= (sv[:orig][:y] + sv[:len])
    )
        steps = sv[:steps] + sh[:steps]
        steps += (sv[:dir] == "U") ?
            (y - sv[:orig][:y]) : (sv[:orig][:y] + sv[:len] - y)
        steps += (sh[:dir] == "R") ?
            (x - sh[:orig][:x]) : (sh[:orig][:x] + sh[:len] - x)
        {x: x, y: y, steps: steps}
    end
end

def intersect(s1, s2)
    # Ignore overlaps, they don't happen in the input.
    if vertical?(s1) && !vertical?(s2)
        intersect_ortho(s1, s2)
    elsif vertical?(s2) && !vertical?(s1)
        intersect_ortho(s2, s1)
    end
end

def lookup(&dist)
    first, second = input_file.readlines.map{|x| segments(parse_line(x))}
    min_d, cur_p = 2**32, nil
    first.each do |s1|
        second.each do |s2|
            pt = intersect(s1, s2)
            next unless pt
            d = dist.call(pt)
            if d > 0 && d < min_d
                min_d, cur_p = d, pt
            end
        end
    end
    min_d
end
