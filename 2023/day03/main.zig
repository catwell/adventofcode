const std = @import("std");

const GearInfo = struct { count: u32, score: u32 };
const Pos = struct { x: usize, y: usize };

const NeighborsIterator = struct {
    x1: usize,
    x2: usize,
    y: usize,
    xl: usize,
    xr: usize,
    below: bool,
    cur_x: ?usize,
    cur_y: ?usize,

    pub fn next(self: *NeighborsIterator) ?Pos {
        if (self.cur_x == null) {
            self.cur_y = self.y;
            self.cur_x = if (self.xl == self.x1) self.xr else self.xl;
            return .{ .x = self.cur_x.?, .y = self.cur_y.? };
        }
        if (self.cur_y == self.y) {
            if (self.cur_x == self.xl) {
                self.cur_x = self.xr;
                if (self.xr != self.x2) {
                    return .{ .x = self.cur_x.?, .y = self.cur_y.? };
                }
            }
            if (self.cur_x == self.xr) {
                self.cur_y = if (self.y == 0) self.y + 1 else self.y - 1;
                self.cur_x = self.xl;
                return .{ .x = self.cur_x.?, .y = self.cur_y.? };
            }
        }
        if (self.cur_x.? < self.xr) {
            self.cur_x = self.cur_x.? + 1;
            return .{ .x = self.cur_x.?, .y = self.cur_y.? };
        }
        if (self.cur_y.? > self.y or !self.below) {
            return null;
        }
        self.cur_y = self.y + 1;
        self.cur_x = self.xl;
        return .{ .x = self.cur_x.?, .y = self.cur_y.? };
    }
};

const Grid = struct {
    buf: [256][256:0]u8,
    width: usize,
    height: usize,

    fn init(self: *Grid) void {
        self.width = 0;
        self.height = 0;
    }

    fn load(self: *Grid) !void {
        var file = try std.fs.cwd().openFile("input.txt", .{});
        defer file.close();

        var buf_reader = std.io.bufferedReader(file.reader());
        var in_stream = buf_reader.reader();

        self.init();

        while (true) {
            var w = try in_stream.readUntilDelimiterOrEof(&self.buf[self.height], '\n');
            if (w == null) {
                break;
            }
            if (self.width == 0) {
                self.width = w.?.len;
            }
            self.height += 1;
        }
    }

    fn neighbors(self: *Grid, x1: usize, x2: usize, y: usize) NeighborsIterator {
        const xl = if (x1 > 0) x1 - 1 else x1;
        const xr = if (x2 < self.width - 1) x2 + 1 else x2;
        const below = y < self.height - 1;
        return .{
            .x1 = x1,
            .x2 = x2,
            .y = y,
            .xl = xl,
            .xr = xr,
            .below = below,
            .cur_x = null,
            .cur_y = null,
        };
    }

    fn is_number(self: *Grid, x: usize, y: usize) bool {
        return self.buf[y][x] >= '0' and self.buf[y][x] <= '9';
    }

    fn is_symbol(self: *Grid, x: usize, y: usize) bool {
        return self.buf[y][x] != '.' and !self.is_number(x, y);
    }

    fn is_star(self: *Grid, x: usize, y: usize) bool {
        return self.buf[y][x] == '*';
    }

    fn is_part_number(self: *Grid, x1: usize, x2: usize, y: usize) bool {
        var iter = self.neighbors(x1, x2, y);
        while (iter.next()) |pos| {
            if (self.is_symbol(pos.x, pos.y)) {
                return true;
            }
        }
        return false;
    }

    fn get_part_number(self: *Grid, x1: usize, x2: usize, y: usize) !u32 {
        return try std.fmt.parseUnsigned(u32, self.buf[y][x1 .. x2 + 1], 10);
    }
};

pub fn main() !void {
    var grid: Grid = undefined;
    try grid.load();

    var part1_sum: u32 = 0;

    var gears = std.AutoHashMap(Pos, GearInfo).init(std.heap.page_allocator);
    defer gears.deinit();

    for (0..grid.height) |y| {
        var x: usize = 0;
        while (x < grid.width) {
            if (!grid.is_number(x, y)) {
                x += 1;
                continue;
            }
            var x2 = x;
            while (true) {
                if (x2 == grid.width or !grid.is_number(x2 + 1, y)) break;
                x2 += 1;
            }
            if (grid.is_part_number(x, x2, y)) {
                const n = try grid.get_part_number(x, x2, y);
                part1_sum += n;

                // part 2
                var iter = grid.neighbors(x, x2, y);
                while (iter.next()) |pos| {
                    if (grid.is_star(pos.x, pos.y)) {
                        var g = gears.get(pos);
                        if (g == null) {
                            try gears.put(pos, .{ .count = 1, .score = n });
                        } else {
                            try gears.put(pos, .{ .count = g.?.count + 1, .score = g.?.score * n });
                        }
                    }
                }
            }
            x = x2 + 1;
        }
    }

    var part2_sum: u32 = 0;
    var iter = gears.iterator();
    while (iter.next()) |entry| {
        var g = entry.value_ptr;
        if (g.count == 2) {
            part2_sum += g.score;
        }
    }

    std.debug.print("part 1: {d}\n", .{part1_sum});
    std.debug.print("part 2: {d}\n", .{part2_sum});
}
