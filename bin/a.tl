main :: () {
	nested :: () {
		nested2 :: () {
			a :: 123 + 543 + 111;
			return a;
		}
		return nested2();
	}
	return nested();
}
