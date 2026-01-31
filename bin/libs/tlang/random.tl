import "std.tl"

XorShift32 :: struct {
	v: U32 = 1
};

next_U32 :: (using this: *XorShift32) {
	/* Algorithm "xor" from p. 4 of Marsaglia, "Xorshift RNGs" */
	v ^= v << 13
	v ^= v >> 17
	v ^= v << 5
	return v
}

next_F32 :: (using this: *XorShift32) => (this.next_U32() >> 8) as S32 as F32 * (1.0 / 16777216.0)
