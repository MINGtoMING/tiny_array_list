const std = @import("std");
const debug = std.debug;
const assert = debug.assert;
const testing = std.testing;
const mem = std.mem;
const math = std.math;
const Allocator = mem.Allocator;

/// A contiguous, growable list of items in memory that initially uses a fixed-size
/// inline buffer and automatically falls back to the heap for larger allocations.
///
/// This struct internally stores a `std.mem.Allocator` for memory management.
pub fn TinyArrayList(comptime T: type) type {
    return TinyArrayListAligned(T, null);
}

/// A contiguous, growable list of items in memory that initially uses a fixed-size
/// inline buffer and automatically falls back to the heap for larger allocations.
///
/// This is a wrapper around an array of T values aligned to `alignment`-byte
/// addresses. If the specified alignment is `null`, then `@alignOf(T)` is used.
/// Initialize with `init`.
///
/// This struct internally stores a `std.mem.Allocator` for memory management.
pub fn TinyArrayListAligned(comptime T: type, comptime alignment: ?u29) type {
    if (alignment) |a| {
        if (a == @alignOf(T)) {
            return TinyArrayListAligned(T, null);
        }
    }
    return struct {
        const Self = @This();
        /// Contents of the list. This field is intended to be accessed
        /// directly.
        ///
        /// Pointers to elements in this slice are invalidated by various
        /// functions of this TinyArrayList in accordance with the respective
        /// documentation. In all cases, "invalidated" means that the memory
        /// has been passed to this allocator's resize or free function.
        items: Slice,
        /// How many T values this list can hold without allocating
        /// additional memory.
        capacity: usize,
        /// Pointers to fix-sized inline buffer for initially using.
        buffer: Slice,
        /// The heap allocator for larger allocations.
        allocator: Allocator,

        pub const Slice = if (alignment) |a| ([]align(a) T) else []T;

        /// Deinitialize with `deinit` or use `toOwnedSlice`.
        pub fn init(buffer: Slice, allocator: Allocator) Self {
            @memset(buffer, undefined);
            return Self{
                .items = buffer[0..0],
                .capacity = buffer.len,
                .buffer = buffer,
                .allocator = allocator,
            };
        }

        /// Initialize with capacity to hold `num` elements.
        /// The resulting capacity will equal `num` exactly.
        /// Deinitialize with `deinit` or use `toOwnedSlice`.
        pub fn initCapacity(buffer: Slice, allocator: Allocator, num: usize) Allocator.Error!Self {
            var self = Self.init(buffer, allocator);
            try self.ensureTotalCapacityPrecise(num);
            return self;
        }

        /// Release all allocated memory.
        pub fn deinit(self: Self) void {
            if (@sizeOf(T) > 0 and self.capacity > self.buffer.len)
                self.allocator.free(self.allocatedSlice());
        }

        /// TinyArrayList takes ownership of the passed in slice. The slice must have been
        /// allocated with `allocator`.
        /// Deinitialize with `deinit` or use `toOwnedSlice`.
        pub fn fromOwnedSlice(buffer: Slice, allocator: Allocator, slice: Slice) Self {
            @memset(buffer, undefined);
            if (buffer.len >= slice.len) {
                @memcpy(buffer[0..slice.len], slice);
                defer allocator.free(slice);
                return Self{
                    .items = buffer[0..slice.len],
                    .capacity = buffer.len,
                    .buffer = buffer,
                    .allocator = allocator,
                };
            } else {
                return Self{
                    .items = slice,
                    .capacity = slice.len,
                    .buffer = buffer,
                    .allocator = allocator,
                };
            }
        }

        /// The caller owns the returned memory. Empties this TinyArrayList,
        /// Its capacity is cleared, making deinit() safe but unnecessary to call.
        pub fn toOwnedSlice(self: *Self) Allocator.Error!Slice {
            const allocator = self.allocator;

            if (self.buffer.ptr != self.items.ptr) {
                const old_memory = self.allocatedSlice();
                if (allocator.resize(old_memory, self.items.len)) {
                    const result = self.items;
                    self.* = init(self.buffer, allocator);
                    return result;
                }
            }

            const new_memory = try allocator.alignedAlloc(T, alignment, self.items.len);
            @memcpy(new_memory, self.items);
            @memset(self.items, undefined);
            self.clearAndFree();
            return new_memory;
        }

        /// Creates a copy of this TinyArrayList, using a new buffer and the same allocator.
        pub fn clone(self: Self, buffer: Slice) Allocator.Error!Self {
            assert(buffer.len == self.buffer.len);
            var cloned = try Self.initCapacity(buffer, self.allocator, self.capacity);
            cloned.appendSliceAssumeCapacity(self.items);
            return cloned;
        }

        /// Insert `item` at index `i`. Moves `list[i .. list.len]` to higher indices to make room.
        /// If `i` is equal to the length of the list this operation is equivalent to append.
        /// This operation is O(N).
        /// Invalidates element pointers if additional memory is needed.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn insert(self: *Self, i: usize, item: T) Allocator.Error!void {
            const dst = try self.addManyAt(i, 1);
            dst[0] = item;
        }

        /// Insert `item` at index `i`. Moves `list[i .. list.len]` to higher indices to make room.
        /// If `i` is equal to the length of the list this operation is
        /// equivalent to appendAssumeCapacity.
        /// This operation is O(N).
        /// Asserts that there is enough capacity for the new item.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn insertAssumeCapacity(self: *Self, i: usize, item: T) void {
            assert(self.items.len < self.capacity);
            self.items.len += 1;

            mem.copyBackwards(T, self.items[i + 1 .. self.items.len], self.items[i .. self.items.len - 1]);
            self.items[i] = item;
        }

        /// Add `count` new elements at position `index`, which have
        /// `undefined` values. Returns a slice pointing to the newly allocated
        /// elements, which becomes invalid after various `TinyArrayList`
        /// operations.
        /// Invalidates pre-existing pointers to elements at and after `index`.
        /// Invalidates all pre-existing element pointers if capacity must be
        /// increased to accommodate the new elements.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn addManyAt(self: *Self, index: usize, count: usize) Allocator.Error![]T {
            const new_len = try addOrOom(self.items.len, count);

            if (self.capacity >= new_len)
                return addManyAtAssumeCapacity(self, index, count);

            const new_capacity = growCapacity(self.capacity, new_len);
            if (self.buffer.ptr != self.items.ptr) {
                // Here we avoid copying allocated but unused bytes by
                // attempting a resize in place, and falling back to allocating
                // a new buffer and doing our own copy. With a realloc() call,
                // the allocator implementation would pointlessly copy our
                // extra capacity.
                if (self.allocator.resize(self.allocatedSlice(), new_capacity)) {
                    self.capacity = new_capacity;
                    return addManyAtAssumeCapacity(self, index, count);
                }
            }

            // Make a new allocation, avoiding `ensureTotalCapacity` in order
            // to avoid extra memory copies.
            const new_memory = try self.allocator.alignedAlloc(T, alignment, new_capacity);
            const to_move = self.items[index..];
            @memcpy(new_memory[0..index], self.items[0..index]);
            @memcpy(new_memory[index + count ..][0..to_move.len], to_move);
            if (self.buffer.ptr != self.items.ptr)
                self.allocator.free(self.allocatedSlice());
            self.items = new_memory[0..new_len];
            self.capacity = new_memory.len;
            // The inserted elements at `new_memory[index..][0..count]` have
            // already been set to `undefined` by memory allocation.
            return new_memory[index..][0..count];
        }

        /// Add `count` new elements at position `index`, which have
        /// `undefined` values. Returns a slice pointing to the newly allocated
        /// elements, which becomes invalid after various `TinyArrayList`
        /// operations.
        /// Asserts that there is enough capacity for the new elements.
        /// Invalidates pre-existing pointers to elements at and after `index`, but
        /// does not invalidate any before that.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn addManyAtAssumeCapacity(self: *Self, index: usize, count: usize) []T {
            const new_len = self.items.len + count;
            assert(self.capacity >= new_len);
            const to_move = self.items[index..];
            self.items.len = new_len;
            mem.copyBackwards(T, self.items[index + count ..], to_move);
            const result = self.items[index..][0..count];
            @memset(result, undefined);
            return result;
        }

        /// Insert slice `items` at index `i` by moving `list[i .. list.len]` to make room.
        /// This operation is O(N).
        /// Invalidates pre-existing pointers to elements at and after `index`.
        /// Invalidates all pre-existing element pointers if capacity must be
        /// increased to accommodate the new elements.
        /// Asserts that the index is in bounds or equal to the length.
        pub fn insertSlice(
            self: *Self,
            index: usize,
            items: []const T,
        ) Allocator.Error!void {
            const dst = try self.addManyAt(index, items.len);
            @memcpy(dst, items);
        }

        /// Grows or shrinks the list as necessary.
        /// Invalidates element pointers if additional capacity is allocated.
        /// Asserts that the range is in bounds.
        pub fn replaceRange(self: *Self, start: usize, len: usize, new_items: []const T) Allocator.Error!void {
            const after_range = start + len;
            const range = self.items[start..after_range];
            if (range.len < new_items.len) {
                const first = new_items[0..range.len];
                const rest = new_items[range.len..];
                @memcpy(range[0..first.len], first);
                try self.insertSlice(after_range, rest);
            } else {
                self.replaceRangeAssumeCapacity(start, len, new_items);
            }
        }

        /// Grows or shrinks the list as necessary.
        /// Never invalidates element pointers.
        /// Asserts the capacity is enough for additional items.
        pub fn replaceRangeAssumeCapacity(self: *Self, start: usize, len: usize, new_items: []const T) void {
            assert(self.items.len - len + new_items.len <= self.capacity);
            const after_range = start + len;
            const range = self.items[start..after_range];

            if (range.len == new_items.len)
                @memcpy(range[0..new_items.len], new_items)
            else if (range.len < new_items.len) {
                const first = new_items[0..range.len];
                const rest = new_items[range.len..];
                @memcpy(range[0..first.len], first);
                const dst = self.addManyAtAssumeCapacity(after_range, rest.len);
                @memcpy(dst, rest);
            } else {
                const extra = range.len - new_items.len;
                @memcpy(range[0..new_items.len], new_items);
                std.mem.copyForwards(
                    T,
                    self.items[after_range - extra ..],
                    self.items[after_range..],
                );
                @memset(self.items[self.items.len - extra ..], undefined);
                self.items.len -= extra;
            }
        }

        /// Extends the list by 1 element. Allocates more memory as necessary.
        /// Invalidates element pointers if additional memory is needed.
        pub fn append(self: *Self, item: T) Allocator.Error!void {
            const new_item_ptr = try self.addOne();
            new_item_ptr.* = item;
        }

        /// Extends the list by 1 element.
        /// Never invalidates element pointers.
        /// Asserts that the list can hold one additional item.
        pub fn appendAssumeCapacity(self: *Self, item: T) void {
            const new_item_ptr = self.addOneAssumeCapacity();
            new_item_ptr.* = item;
        }

        /// Remove the element at index `i`, shift elements after index
        /// `i` forward, and return the removed element.
        /// Invalidates element pointers to end of list.
        /// This operation is O(N).
        /// This preserves item order. Use `swapRemove` if order preservation is not important.
        /// Asserts that the index is in bounds.
        /// Asserts that the list is not empty.
        pub fn orderedRemove(self: *Self, i: usize) T {
            const old_item = self.items[i];
            self.replaceRangeAssumeCapacity(i, 1, &.{});
            return old_item;
        }

        /// Removes the element at the specified index and returns it.
        /// The empty slot is filled from the end of the list.
        /// This operation is O(1).
        /// This may not preserve item order. Use `orderedRemove` if you need to preserve order.
        /// Asserts that the list is not empty.
        /// Asserts that the index is in bounds.
        pub fn swapRemove(self: *Self, i: usize) T {
            if (self.items.len - 1 == i) return self.pop();

            const old_item = self.items[i];
            self.items[i] = self.pop();
            return old_item;
        }

        /// Append the slice of items to the list. Allocates more
        /// memory as necessary.
        /// Invalidates element pointers if additional memory is needed.
        pub fn appendSlice(self: *Self, items: []const T) Allocator.Error!void {
            try self.ensureUnusedCapacity(items.len);
            self.appendSliceAssumeCapacity(items);
        }

        /// Append the slice of items to the list.
        /// Never invalidates element pointers.
        /// Asserts that the list can hold the additional items.
        pub fn appendSliceAssumeCapacity(self: *Self, items: []const T) void {
            const old_len = self.items.len;
            const new_len = old_len + items.len;
            assert(new_len <= self.capacity);
            self.items.len = new_len;
            @memcpy(self.items[old_len..][0..items.len], items);
        }

        pub const Writer = if (T != u8)
            @compileError("The Writer interface is only defined for TinyArrayList(u8) " ++
                "but the given type is TinyArrayList(" ++ @typeName(T) ++ ")")
        else
            std.io.Writer(*Self, Allocator.Error, appendWrite);

        /// Initializes a Writer which will append to the list.
        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        /// Same as `append` except it returns the number of bytes written, which is always the same
        /// as `m.len`. The purpose of this function existing is to match `std.io.Writer` API.
        /// Invalidates element pointers if additional memory is needed.
        fn appendWrite(self: *Self, m: []const u8) Allocator.Error!usize {
            try self.appendSlice(m);
            return m.len;
        }

        pub const FixedWriter = std.io.Writer(*Self, Allocator.Error, appendWriteFixed);

        /// Initializes a Writer which will append to the list but will return
        /// `error.OutOfMemory` rather than increasing capacity.
        pub fn fixedWriter(self: *Self) FixedWriter {
            return .{ .context = self };
        }

        /// The purpose of this function existing is to match `std.io.Writer` API.
        fn appendWriteFixed(self: *Self, m: []const u8) error{OutOfMemory}!usize {
            const available_capacity = self.capacity - self.items.len;
            if (m.len > available_capacity)
                return error.OutOfMemory;

            self.appendSliceAssumeCapacity(m);
            return m.len;
        }

        /// Append a value to the list `n` times.
        /// Allocates more memory as necessary.
        /// Invalidates element pointers if additional memory is needed.
        /// The function is inline so that a comptime-known `value` parameter will
        /// have a more optimal memset codegen in case it has a repeated byte pattern.
        pub inline fn appendNTimes(self: *Self, value: T, n: usize) Allocator.Error!void {
            const old_len = self.items.len;
            try self.resize(try addOrOom(old_len, n));
            @memset(self.items[old_len..self.items.len], value);
        }

        /// Append a value to the list `n` times.
        /// Never invalidates element pointers.
        /// The function is inline so that a comptime-known `value` parameter will
        /// have a more optimal memset codegen in case it has a repeated byte pattern.
        /// Asserts that the list can hold the additional items.
        pub inline fn appendNTimesAssumeCapacity(self: *Self, value: T, n: usize) void {
            const new_len = self.items.len + n;
            assert(new_len <= self.capacity);
            @memset(self.items.ptr[self.items.len..new_len], value);
            self.items.len = new_len;
        }

        /// Adjust the list length to `new_len`.
        /// Additional elements contain the value `undefined`.
        /// Invalidates element pointers if additional memory is needed.
        pub fn resize(self: *Self, new_len: usize) Allocator.Error!void {
            try self.ensureTotalCapacity(new_len);
            self.items.len = new_len;
        }

        /// Invalidates all element pointers.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.items.len = 0;
        }

        /// Invalidates all element pointers.
        pub fn clearAndFree(self: *Self) void {
            if (self.buffer.ptr != self.items.ptr)
                self.allocator.free(self.allocatedSlice());
            self.items.len = 0;
            self.capacity = 0;
        }

        /// If the current capacity is less than `new_capacity`, this function will
        /// modify the array so that it can hold at least `new_capacity` items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureTotalCapacity(self: *Self, new_capacity: usize) Allocator.Error!void {
            if (@sizeOf(T) == 0) {
                self.capacity = math.maxInt(usize);
                return;
            }

            if (self.capacity >= new_capacity) return;

            const better_capacity = growCapacity(self.capacity, new_capacity);
            return self.ensureTotalCapacityPrecise(better_capacity);
        }

        /// If the current capacity is less than `new_capacity`, this function will
        /// modify the array so that it can hold exactly `new_capacity` items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureTotalCapacityPrecise(self: *Self, new_capacity: usize) Allocator.Error!void {
            if (@sizeOf(T) == 0) {
                self.capacity = math.maxInt(usize);
                return;
            }

            if (self.capacity >= new_capacity) return;

            if (self.buffer.ptr != self.items.ptr) {
                // Here we avoid copying allocated but unused bytes by
                // attempting a resize in place, and falling back to allocating
                // a new buffer and doing our own copy. With a realloc() call,
                // the allocator implementation would pointlessly copy our
                // extra capacity.
                if (self.allocator.resize(self.allocatedSlice(), new_capacity)) {
                    self.capacity = new_capacity;
                    return;
                }
            }

            const new_memory = try self.allocator.alignedAlloc(T, alignment, new_capacity);
            @memcpy(new_memory[0..self.items.len], self.items);
            if (self.buffer.ptr != self.items.ptr)
                self.allocator.free(self.allocatedSlice());
            self.items.ptr = new_memory.ptr;
            self.capacity = new_memory.len;
        }

        /// Modify the array so that it can hold at least `additional_count` **more** items.
        /// Invalidates element pointers if additional memory is needed.
        pub fn ensureUnusedCapacity(self: *Self, additional_count: usize) Allocator.Error!void {
            return self.ensureTotalCapacity(try addOrOom(self.items.len, additional_count));
        }

        /// Increases the array's length to match the full capacity that is already allocated.
        /// The new elements have `undefined` values.
        /// Never invalidates element pointers.
        pub fn expandToCapacity(self: *Self) void {
            self.items.len = self.capacity;
        }

        /// Increase length by 1, returning pointer to the new item.
        /// The returned pointer becomes invalid when the list resized.
        pub fn addOne(self: *Self) Allocator.Error!*T {
            // This can never overflow because `self.items` can never occupy the whole address space
            const newlen = self.items.len + 1;
            try self.ensureTotalCapacity(newlen);
            return self.addOneAssumeCapacity();
        }

        /// Increase length by 1, returning pointer to the new item.
        /// The returned pointer becomes invalid when the list is resized.
        /// Never invalidates element pointers.
        /// Asserts that the list can hold one additional item.
        pub fn addOneAssumeCapacity(self: *Self) *T {
            assert(self.items.len < self.capacity);
            self.items.len += 1;
            return &self.items[self.items.len - 1];
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is an array pointing to the newly allocated elements.
        /// The returned pointer becomes invalid when the list is resized.
        /// Resizes list if `self.capacity` is not large enough.
        pub fn addManyAsArray(self: *Self, comptime n: usize) Allocator.Error!*[n]T {
            const prev_len = self.items.len;
            try self.resize(try addOrOom(self.items.len, n));
            return self.items[prev_len..][0..n];
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is an array pointing to the newly allocated elements.
        /// Never invalidates element pointers.
        /// The returned pointer becomes invalid when the list is resized.
        /// Asserts that the list can hold the additional items.
        pub fn addManyAsArrayAssumeCapacity(self: *Self, comptime n: usize) *[n]T {
            assert(self.items.len + n <= self.capacity);
            const prev_len = self.items.len;
            self.items.len += n;
            return self.items[prev_len..][0..n];
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is a slice pointing to the newly allocated elements.
        /// The returned pointer becomes invalid when the list is resized.
        /// Resizes list if `self.capacity` is not large enough.
        pub fn addManyAsSlice(self: *Self, n: usize) Allocator.Error![]T {
            const prev_len = self.items.len;
            try self.resize(try addOrOom(self.items.len, n));
            return self.items[prev_len..][0..n];
        }

        /// Resize the array, adding `n` new elements, which have `undefined` values.
        /// The return value is a slice pointing to the newly allocated elements.
        /// Never invalidates element pointers.
        /// The returned pointer becomes invalid when the list is resized.
        /// Asserts that the list can hold the additional items.
        pub fn addManyAsSliceAssumeCapacity(self: *Self, n: usize) []T {
            assert(self.items.len + n <= self.capacity);
            const prev_len = self.items.len;
            self.items.len += n;
            return self.items[prev_len..][0..n];
        }

        /// Remove and return the last element from the list.
        /// Invalidates element pointers to the removed element.
        /// Asserts that the list is not empty.
        pub fn pop(self: *Self) T {
            const val = self.items[self.items.len - 1];
            self.items.len -= 1;
            return val;
        }

        /// Remove and return the last element from the list, or
        /// return `null` if list is empty.
        /// Invalidates element pointers to the removed element, if any.
        pub fn popOrNull(self: *Self) ?T {
            if (self.items.len == 0) return null;
            return self.pop();
        }

        /// Returns a slice of all the items plus the extra capacity, whose memory
        /// contents are `undefined`.
        pub fn allocatedSlice(self: Self) Slice {
            // `items.len` is the length, not the capacity.
            return self.items.ptr[0..self.capacity];
        }

        /// Returns a slice of only the extra capacity after items.
        /// This can be useful for writing directly into an TinyArrayList.
        /// Note that such an operation must be followed up with a direct
        /// modification of `self.items.len`.
        pub fn unusedCapacitySlice(self: Self) []T {
            return self.allocatedSlice()[self.items.len..];
        }

        /// Returns the last element from the list.
        /// Asserts that the list is not empty.
        pub fn getLast(self: Self) T {
            const val = self.items[self.items.len - 1];
            return val;
        }

        /// Returns the last element from the list, or `null` if list is empty.
        pub fn getLastOrNull(self: Self) ?T {
            if (self.items.len == 0) return null;
            return self.getLast();
        }
    };
}

/// Called when memory growth is necessary. Returns a capacity larger than
/// minimum that grows super-linearly.
fn growCapacity(current: usize, minimum: usize) usize {
    var new = current;
    while (true) {
        new +|= new / 2 + 8;
        if (new >= minimum)
            return new;
    }
}

/// Integer addition returning `error.OutOfMemory` on overflow.
fn addOrOom(a: usize, b: usize) error{OutOfMemory}!usize {
    const result, const overflow = @addWithOverflow(a, b);
    if (overflow != 0) return error.OutOfMemory;
    return result;
}

test "TinyArrayList.init" {
    var buffer: [4]i32 = undefined;
    var list = TinyArrayList(i32).init(&buffer, testing.allocator);
    defer list.deinit();
    try testing.expect(list.items.len == 0);
    try testing.expect(list.items.ptr == list.buffer.ptr);
    try testing.expect(list.capacity == list.buffer.len);
}

test "TinyArrayList.initCapacity" {
    {
        var buffer: [4]i8 = undefined;
        var list = try TinyArrayList(i8).initCapacity(&buffer, testing.allocator, 2);
        defer list.deinit();
        try testing.expect(list.items.len == 0);
        try testing.expect(list.items.ptr == list.buffer.ptr);
        try testing.expect(list.capacity == 4);
    }
    {
        var buffer: [4]i8 = undefined;
        var list = try TinyArrayList(i8).initCapacity(&buffer, testing.allocator, 8);
        defer list.deinit();
        try testing.expect(list.items.len == 0);
        try testing.expect(list.items.ptr != list.buffer.ptr);
        try testing.expect(list.capacity >= 8);
    }
}

test "TinyArrayList.clone" {
    {
        var buffer: [4]i32 = undefined;
        var array = TinyArrayList(i32).init(&buffer, testing.allocator);
        try array.append(-1);
        try array.append(3);
        try array.append(5);

        var new_buffer: [buffer.len]i32 = undefined;
        const cloned = try array.clone(&new_buffer);
        defer cloned.deinit();

        try testing.expectEqualSlices(i32, array.items, cloned.items);
        try testing.expectEqual(array.allocator, cloned.allocator);
        try testing.expect(cloned.capacity >= array.capacity);

        array.deinit();

        try testing.expectEqual(@as(i32, -1), cloned.items[0]);
        try testing.expectEqual(@as(i32, 3), cloned.items[1]);
        try testing.expectEqual(@as(i32, 5), cloned.items[2]);
    }
    {
        var buffer: [2]i32 = undefined;
        var array = TinyArrayList(i32).init(&buffer, testing.allocator);
        try array.append(-1);
        try array.append(3);
        try array.append(5);

        var new_buffer: [buffer.len]i32 = undefined;
        const cloned = try array.clone(&new_buffer);
        defer cloned.deinit();

        try testing.expectEqualSlices(i32, array.items, cloned.items);
        try testing.expectEqual(array.allocator, cloned.allocator);
        try testing.expect(cloned.capacity >= array.capacity);

        array.deinit();

        try testing.expectEqual(@as(i32, -1), cloned.items[0]);
        try testing.expectEqual(@as(i32, 3), cloned.items[1]);
        try testing.expectEqual(@as(i32, 5), cloned.items[2]);
    }
}

test "TinyArrayList.append & TinyArrayList.appendSlice & TinyArrayList.pop" {
    {
        var buffer: [10]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        {
            var i: usize = 0;
            while (i < 10) : (i += 1) {
                list.append(@as(i32, @intCast(i + 1))) catch unreachable;
            }
        }

        {
            var i: usize = 0;
            while (i < 10) : (i += 1) {
                try testing.expect(list.items[i] == @as(i32, @intCast(i + 1)));
            }
        }

        for (list.items, 0..) |v, i| {
            try testing.expect(v == @as(i32, @intCast(i + 1)));
        }

        try testing.expect(list.pop() == 10);
        try testing.expect(list.items.len == 9);

        list.appendSlice(&[_]i32{ 1, 2, 3 }) catch unreachable;
        try testing.expect(list.items.len == 12);
        try testing.expect(list.pop() == 3);
        try testing.expect(list.pop() == 2);
        try testing.expect(list.pop() == 1);
        try testing.expect(list.items.len == 9);

        list.appendSlice(&[_]i32{}) catch unreachable;
        try testing.expect(list.items.len == 9);

        // can only set on indices < self.items.len
        list.items[7] = 33;
        list.items[8] = 42;

        try testing.expect(list.pop() == 42);
        try testing.expect(list.pop() == 33);
    }
    {
        var buffer: [2]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        {
            var i: usize = 0;
            while (i < 10) : (i += 1) {
                list.append(@as(i32, @intCast(i + 1))) catch unreachable;
            }
        }

        {
            var i: usize = 0;
            while (i < 10) : (i += 1) {
                try testing.expect(list.items[i] == @as(i32, @intCast(i + 1)));
            }
        }

        for (list.items, 0..) |v, i| {
            try testing.expect(v == @as(i32, @intCast(i + 1)));
        }

        try testing.expect(list.pop() == 10);
        try testing.expect(list.items.len == 9);

        list.appendSlice(&[_]i32{ 1, 2, 3 }) catch unreachable;
        try testing.expect(list.items.len == 12);
        try testing.expect(list.pop() == 3);
        try testing.expect(list.pop() == 2);
        try testing.expect(list.pop() == 1);
        try testing.expect(list.items.len == 9);

        list.appendSlice(&[_]i32{}) catch unreachable;
        try testing.expect(list.items.len == 9);

        // can only set on indices < self.items.len
        list.items[7] = 33;
        list.items[8] = 42;

        try testing.expect(list.pop() == 42);
        try testing.expect(list.pop() == 33);
    }
}

test "TinyArrayList.appendNTimes" {
    {
        var buffer: [20]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.appendNTimes(2, 10);
        try testing.expectEqual(@as(usize, 10), list.items.len);
        for (list.items) |element| {
            try testing.expectEqual(@as(i32, 2), element);
        }
    }
    {
        var buffer: [5]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.appendNTimes(2, 10);
        try testing.expectEqual(@as(usize, 10), list.items.len);
        for (list.items) |element| {
            try testing.expectEqual(@as(i32, 2), element);
        }
    }
}

test "TinyArrayList.appendNTimes with failing allocator" {
    {
        var buffer: [10]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.failing_allocator);
        defer list.deinit();
        try testing.expectError(error.OutOfMemory, list.appendNTimes(2, 20));
    }
    {
        var buffer: [10]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.failing_allocator);
        defer list.deinit();
        list.appendNTimes(2, 10) catch unreachable;
    }
}

test "TinyArrayList.orderedRemove" {
    {
        var buffer: [8]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(1);
        try list.append(2);
        try list.append(3);
        try list.append(4);
        try list.append(5);
        try list.append(6);
        try list.append(7);

        //remove from middle
        try testing.expectEqual(@as(i32, 4), list.orderedRemove(3));
        try testing.expectEqual(@as(i32, 5), list.items[3]);
        try testing.expectEqual(@as(usize, 6), list.items.len);

        //remove from end
        try testing.expectEqual(@as(i32, 7), list.orderedRemove(5));
        try testing.expectEqual(@as(usize, 5), list.items.len);

        //remove from front
        try testing.expectEqual(@as(i32, 1), list.orderedRemove(0));
        try testing.expectEqual(@as(i32, 2), list.items[0]);
        try testing.expectEqual(@as(usize, 4), list.items.len);
    }
    {
        var buffer: [4]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(1);
        try list.append(2);
        try list.append(3);
        try list.append(4);
        try list.append(5);
        try list.append(6);
        try list.append(7);

        //remove from middle
        try testing.expectEqual(@as(i32, 4), list.orderedRemove(3));
        try testing.expectEqual(@as(i32, 5), list.items[3]);
        try testing.expectEqual(@as(usize, 6), list.items.len);

        //remove from end
        try testing.expectEqual(@as(i32, 7), list.orderedRemove(5));
        try testing.expectEqual(@as(usize, 5), list.items.len);

        //remove from front
        try testing.expectEqual(@as(i32, 1), list.orderedRemove(0));
        try testing.expectEqual(@as(i32, 2), list.items[0]);
        try testing.expectEqual(@as(usize, 4), list.items.len);
    }
}

test "TinyArrayList.swapRemove" {
    {
        var buffer: [8]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(1);
        try list.append(2);
        try list.append(3);
        try list.append(4);
        try list.append(5);
        try list.append(6);
        try list.append(7);

        //remove from middle
        try testing.expect(list.swapRemove(3) == 4);
        try testing.expect(list.items[3] == 7);
        try testing.expect(list.items.len == 6);

        //remove from end
        try testing.expect(list.swapRemove(5) == 6);
        try testing.expect(list.items.len == 5);

        //remove from front
        try testing.expect(list.swapRemove(0) == 1);
        try testing.expect(list.items[0] == 5);
        try testing.expect(list.items.len == 4);
    }
    {
        var buffer: [4]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(1);
        try list.append(2);
        try list.append(3);
        try list.append(4);
        try list.append(5);
        try list.append(6);
        try list.append(7);

        //remove from middle
        try testing.expect(list.swapRemove(3) == 4);
        try testing.expect(list.items[3] == 7);
        try testing.expect(list.items.len == 6);

        //remove from end
        try testing.expect(list.swapRemove(5) == 6);
        try testing.expect(list.items.len == 5);

        //remove from front
        try testing.expect(list.swapRemove(0) == 1);
        try testing.expect(list.items[0] == 5);
        try testing.expect(list.items.len == 4);
    }
}

test "TinyArrayList.insert" {
    {
        var buffer: [4]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.insert(0, 1);
        try list.append(2);
        try list.insert(2, 3);
        try list.insert(0, 5);
        try testing.expect(list.items[0] == 5);
        try testing.expect(list.items[1] == 1);
        try testing.expect(list.items[2] == 2);
        try testing.expect(list.items[3] == 3);
    }
    {
        var buffer: [2]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.insert(0, 1);
        try list.append(2);
        try list.insert(2, 3);
        try list.insert(0, 5);
        try testing.expect(list.items[0] == 5);
        try testing.expect(list.items[1] == 1);
        try testing.expect(list.items[2] == 2);
        try testing.expect(list.items[3] == 3);
    }
}

test "TinyArrayList.insertSlice" {
    {
        var buffer: [6]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(1);
        try list.append(2);
        try list.append(3);
        try list.append(4);
        try list.insertSlice(1, &[_]i32{ 9, 8 });
        try testing.expect(list.items[0] == 1);
        try testing.expect(list.items[1] == 9);
        try testing.expect(list.items[2] == 8);
        try testing.expect(list.items[3] == 2);
        try testing.expect(list.items[4] == 3);
        try testing.expect(list.items[5] == 4);

        const items = [_]i32{1};
        try list.insertSlice(0, items[0..0]);
        try testing.expect(list.items.len == 6);
        try testing.expect(list.items[0] == 1);
    }
    {
        var buffer: [3]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(1);
        try list.append(2);
        try list.append(3);
        try list.append(4);
        try list.insertSlice(1, &[_]i32{ 9, 8 });
        try testing.expect(list.items[0] == 1);
        try testing.expect(list.items[1] == 9);
        try testing.expect(list.items[2] == 8);
        try testing.expect(list.items[3] == 2);
        try testing.expect(list.items[4] == 3);
        try testing.expect(list.items[5] == 4);

        const items = [_]i32{1};
        try list.insertSlice(0, items[0..0]);
        try testing.expect(list.items.len == 6);
        try testing.expect(list.items[0] == 1);
    }
}

test "TinyArrayList.replaceRange" {
    {
        var buffer: [3]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(1, 0, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 2, 3, 4, 5 }, list.items);
    }
    {
        var buffer: [6]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(1, 1, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(
            i32,
            &[_]i32{ 1, 0, 0, 0, 3, 4, 5 },
            list.items,
        );
    }
    {
        var buffer: [3]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(1, 2, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 4, 5 }, list.items);
    }
    {
        var buffer: [6]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(1, 3, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 5 }, list.items);
    }
    {
        var buffer: [3]i32 = undefined;
        var list = TinyArrayList(i32).init(&buffer, testing.allocator);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        try list.replaceRange(1, 4, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0 }, list.items);
    }
}

test "TinyArrayList.replaceRangeAssumeCapacity" {
    {
        var buffer: [3]i32 = undefined;
        var list = try TinyArrayList(i32).initCapacity(&buffer, testing.allocator, 10);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 0, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 2, 3, 4, 5 }, list.items);
    }
    {
        var buffer: [10]i32 = undefined;
        var list = try TinyArrayList(i32).initCapacity(&buffer, testing.allocator, 10);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 1, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(
            i32,
            &[_]i32{ 1, 0, 0, 0, 3, 4, 5 },
            list.items,
        );
    }
    {
        var buffer: [3]i32 = undefined;
        var list = try TinyArrayList(i32).initCapacity(&buffer, testing.allocator, 10);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 2, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 4, 5 }, list.items);
    }
    {
        var buffer: [10]i32 = undefined;
        var list = try TinyArrayList(i32).initCapacity(&buffer, testing.allocator, 10);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 3, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0, 5 }, list.items);
    }
    {
        var buffer: [3]i32 = undefined;
        var list = try TinyArrayList(i32).initCapacity(&buffer, testing.allocator, 10);
        defer list.deinit();
        try list.appendSlice(&[_]i32{ 1, 2, 3, 4, 5 });

        list.replaceRangeAssumeCapacity(1, 4, &[_]i32{ 0, 0, 0 });

        try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 0, 0 }, list.items);
    }
}

const Item = struct {
    integer: i32,
    sub_items: TinyArrayList(Item),
};

test "TinyArrayList(T) of struct T" {
    {
        var root_buffer: [0]Item = undefined;
        var root = Item{ .integer = 1, .sub_items = .init(&root_buffer, testing.allocator) };
        defer root.sub_items.deinit();
        var sub_buffer: [0]Item = undefined;
        try root.sub_items.append(Item{ .integer = 42, .sub_items = .init(&sub_buffer, testing.allocator) });
        try testing.expect(root.sub_items.items[0].integer == 42);
    }
    {
        var root_buffer: [10]Item = undefined;
        var root = Item{ .integer = 1, .sub_items = .init(&root_buffer, testing.allocator) };
        defer root.sub_items.deinit();
        var sub_buffer: [10]Item = undefined;
        try root.sub_items.append(Item{ .integer = 42, .sub_items = .init(&sub_buffer, testing.allocator) });
        try testing.expect(root.sub_items.items[0].integer == 42);
    }
}

test "TinyArrayList(u8) implements writer" {
    {
        var buffer: [20]u8 align(2) = undefined;
        var list = TinyArrayList(u8).init(&buffer, testing.allocator);
        defer list.deinit();

        const x: i32 = 42;
        const y: i32 = 1234;
        try list.writer().print("x: {}\ny: {}\n", .{ x, y });

        try testing.expectEqualSlices(u8, "x: 42\ny: 1234\n", list.items);
    }
    {
        var buffer: [2]u8 align(2) = undefined;
        var list = TinyArrayListAligned(u8, 2).init(&buffer, testing.allocator);
        defer list.deinit();

        const writer = list.writer();
        try writer.writeAll("a");
        try writer.writeAll("bc");
        try writer.writeAll("d");
        try writer.writeAll("efg");

        try testing.expectEqualSlices(u8, list.items, "abcdefg");
    }
}

test "TinyArrayList.addManyAsArray" {
    {
        var buffer: [10]u8 = undefined;
        var list = TinyArrayList(u8).init(&buffer, testing.allocator);
        defer list.deinit();

        (try list.addManyAsArray(4)).* = "aoeu".*;
        try list.ensureTotalCapacity(8);
        list.addManyAsArrayAssumeCapacity(4).* = "asdf".*;

        try testing.expectEqualSlices(u8, list.items, "aoeuasdf");
    }
    {
        var buffer: [2]u8 = undefined;
        var list = TinyArrayList(u8).init(&buffer, testing.allocator);
        defer list.deinit();

        (try list.addManyAsArray(4)).* = "aoeu".*;
        try list.ensureTotalCapacity(8);
        list.addManyAsArrayAssumeCapacity(4).* = "asdf".*;

        try testing.expectEqualSlices(u8, list.items, "aoeuasdf");
    }
}

test "TinyArrayList.fromOwnedSlice" {
    {
        var orig_buffer: [10]u8 = undefined;
        var orig_list = TinyArrayList(u8).init(&orig_buffer, testing.allocator);
        defer orig_list.deinit();
        try orig_list.appendSlice("foobar");

        const slice = try orig_list.toOwnedSlice();
        var buffer: [10]u8 = undefined;
        var list = TinyArrayList(u8).fromOwnedSlice(&buffer, testing.allocator, slice);
        defer list.deinit();
        try testing.expectEqualStrings(list.items, "foobar");
    }
    {
        var orig_buffer: [2]u8 = undefined;
        var orig_list = TinyArrayList(u8).init(&orig_buffer, testing.allocator);
        defer orig_list.deinit();
        try orig_list.appendSlice("foobar");

        const slice = try orig_list.toOwnedSlice();
        var buffer: [2]u8 = undefined;
        var list = TinyArrayList(u8).fromOwnedSlice(&buffer, testing.allocator, slice);
        defer list.deinit();
        try testing.expectEqualStrings(list.items, "foobar");
    }
}

test "accepts unaligned slices" {
    {
        var buffer: [10]u8 = undefined;
        var list = TinyArrayList(u8).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.appendSlice(&.{ 0, 1, 2, 3 });
        try list.insertSlice(2, &.{ 4, 5, 6, 7 });
        try list.replaceRange(1, 3, &.{ 8, 9 });

        try testing.expectEqualSlices(u8, list.items, &.{ 0, 8, 9, 6, 7, 2, 3 });
    }
    {
        var buffer: [2]u8 = undefined;
        var list = TinyArrayList(u8).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.appendSlice(&.{ 0, 1, 2, 3 });
        try list.insertSlice(2, &.{ 4, 5, 6, 7 });
        try list.replaceRange(1, 3, &.{ 8, 9 });

        try testing.expectEqualSlices(u8, list.items, &.{ 0, 8, 9, 6, 7, 2, 3 });
    }
}

test "TinyArrayList(u0)" {
    // An TinyArrayList on zero-sized types should not need to allocate
    var buffer: [2]u0 = undefined;
    var list = TinyArrayList(u0).init(&buffer, testing.failing_allocator);
    defer list.deinit();

    try list.append(0);
    try list.append(0);
    try list.append(0);
    try testing.expectEqual(list.items.len, 3);

    var count: usize = 0;
    for (list.items) |x| {
        try testing.expectEqual(x, 0);
        count += 1;
    }
    try testing.expectEqual(count, 3);
}

test "TinyArrayList(?u32).popOrNull()" {
    {
        var buffer: [10]?u32 = undefined;
        var list = TinyArrayList(?u32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(null);
        try list.append(1);
        try list.append(2);
        try testing.expectEqual(list.items.len, 3);

        try testing.expect(list.popOrNull().? == @as(u32, 2));
        try testing.expect(list.popOrNull().? == @as(u32, 1));
        try testing.expect(list.popOrNull().? == null);
        try testing.expect(list.popOrNull() == null);
    }
    {
        var buffer: [1]?u32 = undefined;
        var list = TinyArrayList(?u32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(null);
        try list.append(1);
        try list.append(2);
        try testing.expectEqual(list.items.len, 3);

        try testing.expect(list.popOrNull().? == @as(u32, 2));
        try testing.expect(list.popOrNull().? == @as(u32, 1));
        try testing.expect(list.popOrNull().? == null);
        try testing.expect(list.popOrNull() == null);
    }
}

test "TinyArrayList(u32).getLast()" {
    {
        var buffer: [10]u32 = undefined;
        var list = TinyArrayList(u32).init(&buffer, testing.allocator);
        defer list.deinit();

        try list.append(2);
        const const_list = list;
        try testing.expectEqual(const_list.getLast(), 2);
    }
    {
        var buffer: [10]u32 = undefined;
        var list = TinyArrayList(u32).init(&buffer, testing.allocator);
        defer list.deinit();

        for (0..20) |_| {
            try list.append(1);
        }
        try list.append(2);
        const const_list = list;
        try testing.expectEqual(const_list.getLast(), 2);
    }
}

test "TinyArrayList(u32).getLastOrNull()" {
    {
        var buffer: [10]u32 = undefined;
        var list = TinyArrayList(u32).init(&buffer, testing.allocator);
        defer list.deinit();

        try testing.expectEqual(list.getLastOrNull(), null);

        try list.append(2);
        const const_list = list;
        try testing.expectEqual(const_list.getLastOrNull().?, 2);
    }
    {
        var buffer: [10]u32 = undefined;
        var list = TinyArrayList(u32).init(&buffer, testing.allocator);
        defer list.deinit();

        try testing.expectEqual(list.getLastOrNull(), null);
        for (0..20) |_| {
            try list.append(1);
        }
        try list.append(2);
        const const_list = list;
        try testing.expectEqual(const_list.getLastOrNull().?, 2);
    }
}

test "return OutOfMemory when capacity would exceed maximum usize integer value" {
    const a = testing.allocator;
    const new_item: u32 = 42;
    const items = &.{ 42, 43 };

    {
        var buffer: [10]u32 = undefined;
        var list: TinyArrayList(u32) = .{
            .items = undefined,
            .capacity = math.maxInt(usize) - 1,
            .buffer = &buffer,
            .allocator = a,
        };
        list.items.len = math.maxInt(usize) - 1;

        try testing.expectError(error.OutOfMemory, list.appendSlice(items));
        try testing.expectError(error.OutOfMemory, list.appendNTimes(new_item, 2));
        try testing.expectError(error.OutOfMemory, list.addManyAt(0, 2));
        try testing.expectError(error.OutOfMemory, list.addManyAsArray(2));
        try testing.expectError(error.OutOfMemory, list.addManyAsSlice(2));
        try testing.expectError(error.OutOfMemory, list.insertSlice(0, items));
        try testing.expectError(error.OutOfMemory, list.ensureUnusedCapacity(2));
    }
}

test "TinyArrayListAligned with non-native alignment compiles unusedCapabitySlice" {
    var buffer: [10]u8 align(4) = undefined;
    var list = TinyArrayListAligned(u8, 4).init(&buffer, testing.allocator);
    defer list.deinit();
    try list.appendNTimes(1, 4);
    _ = list.unusedCapacitySlice();
}
