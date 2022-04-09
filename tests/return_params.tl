
foo :: fn (): result: int {
	result = 64;
	result = 32;
	return;
}


main :: fn (): int {
	a := foo();
	return a;
}

// This should return 32
