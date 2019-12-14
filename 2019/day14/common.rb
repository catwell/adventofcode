class Q
    attr_accessor :symbol, :quantity

    def initialize(symbol, quantity)
        @symbol = symbol
        @quantity = quantity
    end

    def self.parse(s)
        c, v = s.split(' ')
        new(v, c.to_i)
    end
end

class R
    attr_accessor :q, :requirements

    def initialize(q, requirements)
        @q = q
        @requirements = requirements
    end

    def symbol
        @q.symbol
    end

    def quantity
        @q.quantity
    end

    def self.parse(l)
        ins, outs = l.split(' => ')
        q = Q.parse(outs)
        reqs = ins.split(', ').map { |x| Q.parse(x) }
        new(q, reqs)
    end
end

def parse
    input_file.each.map do |l|
        r = R.parse(l)
        [r.symbol, r]
    end.to_h
end

class State
    attr_accessor :needs, :available, :ore, :reactions

    def initialize(reactions)
        @reactions = reactions
        @needs = Hash.new(0)
        @available = Hash.new(0)
        @ore = 0
    end

    def fulfill_needs
        until needs.values.reduce(:+) == 0
            sym, q = needs.reject { |_, v| v == 0 }.first
            if available[sym] >= q
                available[sym] -= q
            else
                q -= available[sym]
                available[sym] = 0
                r = reactions[sym]
                c = (q / r.quantity.to_f).ceil
                r.requirements.each do |req|
                    if req.symbol == 'ORE'
                        @ore += c * req.quantity
                    else
                        needs[req.symbol] += c * req.quantity
                    end
                end
                available[sym] = c * r.quantity - q
            end
            needs[sym] = 0
        end
    end
end
