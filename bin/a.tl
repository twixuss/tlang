foo :: fn (val: u64) -> s64 {
	return val as s64;
}
main :: fn () -> s64 {
	return foo(-1 as u64);
}