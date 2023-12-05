// AOC 2023 day 5 in Zig

const std = @import("std");
const parseInt = std.fmt.parseInt;

const Range = struct {
    start: u64,
    len: u64,
    mapped: bool,
    next: ?*Range,
};

fn doMap(allocator: std.mem.Allocator, ranges: ?*Range, lines: *std.mem.SplitIterator(u8, .scalar)) !void {
    var it3 = ranges;
    while (it3) |range| : (it3 = range.next) { range.*.mapped = false; }

    _ = lines.next().?;
    while (lines.next()) |line| {
        if (line.len == 0) break;
        var it = std.mem.tokenizeAny(u8, line, " ");
        const dst = try parseInt(u64, it.next().?, 10);
        const src = try parseInt(u64, it.next().?, 10);
        const len = try parseInt(u64, it.next().?, 10);

        const diff = @as(i64, @intCast(dst)) - @as(i64, @intCast(src));
        var it2 = ranges;
        while (it2) |range| : (it2 = range.next) {
            if (range.*.mapped) continue;

            // If src begins in middle of range: Need to split
            if (range.*.start < src and src < range.*.start + range.*.len) {
                const new = try allocator.create(Range);
                new.* = .{
                    .start = range.*.start, .len = src - range.*.start,
                    .mapped = false, .next = range.*.next
                };
                range.*.next = new;
                range.*.start = src;
                range.*.len -= new.*.len;
            }
            // If src+len ends in middle of range: Need to split
            if (range.*.start < src + len and src + len < range.*.start + range.*.len) {
                const new = try allocator.create(Range);
                new.* = .{
                    .start = src + len, .len = range.*.start + range.*.len - (src + len),
                    .mapped = false, .next = range.*.next
                };
                range.*.next = new;
                range.*.len -= new.*.len;
            }

            if (src <= range.*.start and range.*.start + range.*.len <= src + len) {
                range.*.start = @intCast(@as(i64, @intCast(range.*.start)) + diff);
                range.*.mapped = true;
            }
        }
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const input = try std.fs.cwd().readFileAlloc(allocator, "input", 100000);
    var lines = std.mem.splitScalar(u8, input, '\n');

    var xs: ?*Range = null;
    var it = std.mem.tokenizeAny(u8, lines.next().?, " ");
    _ = it.next().?;
    while (it.next()) |num| {
        const start = try parseInt(u64, num, 10);
        // const len = 1; // Part 1
        const len = try parseInt(u64, it.next().?, 10); // Part 2
        const new = try allocator.create(Range);
        new.* = .{ .start = start, .len = len, .mapped = false, .next = xs };
        xs = new;
    }
    _ = lines.next().?;

    try doMap(allocator, xs, &lines); // seed-to-soil map
    try doMap(allocator, xs, &lines); // soil-to-fertilizer map
    try doMap(allocator, xs, &lines); // fertilizer-to-water map
    try doMap(allocator, xs, &lines); // water-to-light map
    try doMap(allocator, xs, &lines); // light-to-temperature map
    try doMap(allocator, xs, &lines); // temperature-to-humidity map
    try doMap(allocator, xs, &lines); // humidity-to-location map

    var min: u64 = std.math.maxInt(u64);
    while (xs) |range| : (xs = range.next) {
        if (range.start < min) { min = range.start; }
    }
    std.debug.print("Min: {any}!\n", .{min});
}
