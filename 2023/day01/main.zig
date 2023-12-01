const std = @import("std");

fn part1_score(line: []const u8) u16 {
    var c1: u8 = 0;
    var c2: u8 = 0;
    for (line) |c| {
        if (c >= '0' and c <= '9') {
            if (c1 == 0) c1 = c;
            c2 = c;
        }
    }
    if (c1 == 0 or c2 == 0) return 0;
    return 10 * (c1 - '0') + (c2 - '0');
}

const lits = [_](struct { s: []const u8, d: u8 }){
    .{ .s = "0", .d = 0 },
    .{ .s = "1", .d = 1 },
    .{ .s = "2", .d = 2 },
    .{ .s = "3", .d = 3 },
    .{ .s = "4", .d = 4 },
    .{ .s = "5", .d = 5 },
    .{ .s = "6", .d = 6 },
    .{ .s = "7", .d = 7 },
    .{ .s = "8", .d = 8 },
    .{ .s = "9", .d = 9 },
    .{ .s = "zero", .d = 0 },
    .{ .s = "one", .d = 1 },
    .{ .s = "two", .d = 2 },
    .{ .s = "three", .d = 3 },
    .{ .s = "four", .d = 4 },
    .{ .s = "five", .d = 5 },
    .{ .s = "six", .d = 6 },
    .{ .s = "seven", .d = 7 },
    .{ .s = "eight", .d = 8 },
    .{ .s = "nine", .d = 9 },
};

const rev_lits = init_rev_lits: {
    var r: [20](struct { s: [16:0]u8, d: u8 }) = undefined;
    inline for (lits, 0..) |v, i| {
        r[i] = .{ .s = undefined, .d = v.d };
        @memset(&r[i].s, 0);
        @memcpy(r[i].s[0..v.s.len], v.s);
        std.mem.reverse(u8, &r[i].s);
    }
    break :init_rev_lits r;
};

fn part2_score(line: []const u8) u16 {
    var v1: ?u8 = null;
    var v2: ?u8 = null;

    var p1: usize = 1024;
    var p2: usize = 1024;

    for (lits) |v| {
        var p = std.mem.indexOf(u8, line, v.s);
        if (p != null and p.? < p1) {
            p1 = p.?;
            v1 = v.d;
        }
    }

    var rev_line_mem: [1024]u8 = undefined;
    @memset(&rev_line_mem, 0);
    @memcpy(rev_line_mem[0..line.len], line);
    var rev_line = std.mem.span(@as([*:0]u8, @ptrCast(&rev_line_mem)));
    std.mem.reverse(u8, rev_line);

    for (rev_lits) |v| {
        const needle = std.mem.span(@as([*:0]u8, @constCast(&v.s)));
        var p = std.mem.indexOf(u8, rev_line, needle);
        if (p != null and p.? < p2) {
            p2 = p.?;
            v2 = v.d;
        }
    }

    return 10 * v1.? + v2.?;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("part1.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var part1_sum: u32 = 0;
    var part2_sum: u32 = 0;
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        part1_sum += part1_score(line);
        part2_sum += part2_score(line);
    }

    std.debug.print("part 1: {d}\n", .{part1_sum});
    std.debug.print("part 2: {d}\n", .{part2_sum});
}
