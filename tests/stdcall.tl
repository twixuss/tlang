a :: #stdcall (x: int, y: int, z: int, w: int): int => x;
b :: #stdcall (x: int, y: int, z: int, w: int): int => y;
c :: #stdcall (x: int, y: int, z: int, w: int): int => z;
d :: #stdcall (x: int, y: int, z: int, w: int): int => w;

main :: (): int {
	return 
		a( 1,  2,  3,  4) +
		b( 5,  6,  7,  8) +
		c( 9, 10, 11, 12) +
		d(13, 14, 15, 16);
}

// Should return 34
