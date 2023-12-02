const std = @import("std");

fn part1_score(line: []const u8) !u32 {
    var iter1 = std.mem.tokenizeAny(u8, line, ":;,");
    var id: ?u32 = null;
    var valid = true;
    while (iter1.next()) |word1| {
        if (word1[0] == ' ') {
            var iter2 = std.mem.tokenizeAny(u8, word1, " ");
            const ns = iter2.next().?;
            const color = iter2.next().?;
            const n = try std.fmt.parseUnsigned(u32, ns, 10);
            valid = valid and switch (color[0]) {
                'r' => n <= 12,
                'g' => n <= 13,
                'b' => n <= 14,
                else => false,
            };
        } else if (word1[0] == 'G') {
            var iter2 = std.mem.tokenizeAny(u8, word1, " ");
            _ = iter2.next().?;
            const idn = iter2.next().?;
            id = try std.fmt.parseUnsigned(u32, idn, 10);
        }
    }
    return if (valid) id.? else 0;
}

fn part2_score(line: []const u8) !u32 {
    var iter1 = std.mem.tokenizeAny(u8, line, ":;,");
    var r = std.mem.zeroes([3]u32);
    while (iter1.next()) |word1| {
        if (word1[0] == ' ') {
            var iter2 = std.mem.tokenizeAny(u8, word1, " ");
            const ns = iter2.next().?;
            const color = iter2.next().?;
            const n = try std.fmt.parseUnsigned(u32, ns, 10);
            const pos: u8 = switch (color[0]) {
                'r' => 0,
                'g' => 1,
                'b' => 2,
                else => 0,
            };
            if (n > r[pos]) {
                r[pos] = n;
            }
        }
    }
    return r[0] * r[1] * r[2];
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var part1_sum: u32 = 0;
    var part2_sum: u32 = 0;
    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        part1_sum += try part1_score(line);
        part2_sum += try part2_score(line);
    }

    std.debug.print("part 1: {d}\n", .{part1_sum});
    std.debug.print("part 2: {d}\n", .{part2_sum});
}
