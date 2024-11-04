const std = @import("std");
const tal = @import("tiny_array_list");

const Gender = enum { Male, Female };

const Person = struct {
    name: []const u8,
    age: u8,
    gender: Gender,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    var buffer: [10]Person = undefined;
    var list = tal.TinyArrayList(Person).init(&buffer, allocator);
    defer list.deinit();

    try list.append(Person{ .name = "ZhangSan", .age = 18, .gender = .Male });
    try list.append(Person{ .name = "LiHua", .age = 25, .gender = .Female });

    try list.insert(1, Person{ .name = "LiSi", .age = 27, .gender = .Male });

    const removed = list.orderedRemove(0);

    for (list.items) |person| {
        std.debug.print("Name: {s}, Age: {}, Gender: {}\n", .{ person.name, person.age, person.gender });
    }

    std.debug.print("Removed person: Name: {s}, Age: {}, Gender: {}\n", .{ removed.name, removed.age, removed.gender });
}
