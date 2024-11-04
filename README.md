# Tiny Array List

`TinyArrayList` is a modified version of `std.ArrayList` that initially uses a fixed-size inline buffer for data
storage. It only transitions to heap memory when the data size surpasses the buffer capacity, effectively minimizing
heap allocation overhead for small data sizes.

## License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>
