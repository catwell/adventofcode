def angles_q1(max)
    s = []
    1.upto(max) do |x|
        1.upto(x - 1) do |y|
            s |= [Rational(x, y)]
        end
    end
    s
end

def angles(max)
    base = angles_q1(max).sort.map { |r| [r.numerator, r.denominator] }
    angles = []
    angles << [0, -1]
    angles += base.map { |x, y| [y, -x] }.reverse
    angles << [1, -1]
    angles += base.map { |x, y| [x, -y] }
    angles << [1, 0]
    angles += base.reverse
    angles << [1, 1]
    angles += base.map { |x, y| [y, x] }
    angles << [0, 1]
    angles += base.map { |x, y| [-y, x] }.reverse
    angles << [-1, 1]
    angles += base.map { |x, y| [-x, y] }
    angles << [-1, 0]
    angles += base.map { |x, y| [-x, -y] }.reverse
    angles << [-1, -1]
    angles += base.map { |x, y| [-y, -x] }
    angles
end

def hits_at_angle(space, x0, y0, x, y, x_max, y_max)
    loop do
        x0 += x
        y0 += y
        return false if x0 < 0 || x0 > x_max
        return false if y0 < 0 || y0 > y_max
        return [x0, y0] if space[y0][x0]
    end
end
