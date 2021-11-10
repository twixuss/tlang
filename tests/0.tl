c :: 12;

square :: fn (param : int) => param * param;

main :: fn () {
	a :: (1 + 2) * 3;
	b :: a - 3;

	return 1.max - square(b * c);
}
