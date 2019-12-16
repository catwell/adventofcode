def pattern(pos, len)
    base = []
    (pos + 1).times { base << 0 }
    (pos + 1).times { base << 1 }
    (pos + 1).times { base << 0 }
    (pos + 1).times { base << -1 }
    r = base.dup
    r += base while r.length <= (len + 1)
    r.shift
    r.pop(r.length - len)
    r
end

def phase_at(v, pos)
    ptrn = pattern(pos, v.length)
    v.zip(ptrn).map { |x, y| x * y }.reduce(:+).abs % 10
end

def phase(v)
    (0..v.length - 1).to_a.map { |i| phase_at(v, i) }
end

def fft(v, n)
    n.times { v = phase(v) }
    v
end
