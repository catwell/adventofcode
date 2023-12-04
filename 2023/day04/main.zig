const std = @import("std");

fn score(line: []const u8) !u32 {
    var iter1 = std.mem.tokenizeAny(u8, line, ":|");
    _ = iter1.next().?;
    const winning = iter1.next().?;
    const mine = iter1.next().?;
    var iter2 = std.mem.tokenizeAny(u8, mine, " ");

    var buf: [1024]u8 = undefined;
    var r: u32 = 0;
    while (iter2.next()) |ns| {
        const needle = try std.fmt.bufPrint(&buf, " {s} ", .{ns});
        var p = std.mem.indexOf(u8, winning, needle);
        if (p != null) {
            r += 1;
        }
    }
    return r;
}

pub fn main() !void {
    var file = try std.fs.cwd().openFile("input.txt", .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var part1_sum: u32 = 0;
    var part2_sum: u32 = 0;
    var part2_counts = [_]u32{1} ** 1024;
    var buf: [1024]u8 = undefined;
    var pos: u32 = 0;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var s = try score(line);
        if (s > 0) {
            part1_sum += std.math.shl(u32, 1, s - 1);
            for (pos + 1..pos + s + 1) |p| {
                part2_counts[p] += part2_counts[pos];
            }
        }
        part2_sum += part2_counts[pos];
        pos += 1;
    }

    std.debug.print("part 1: {d}\n", .{part1_sum});
    std.debug.print("part 2: {d}\n", .{part2_sum});
}
