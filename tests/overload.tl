main :: fn () {
	{
		foo :: fn (a: string) => 0;
		foo :: fn (a: $T) => 1;
		foo :: fn (a: any) => 2;
		assert foo("hello") == 0;
	}
	{
		foo :: fn (a: $T) => 1;
		foo :: fn (a: any) => 2;
		assert foo("hello") == 1;
	}
	{
		foo :: fn (a: string) => 0;
		foo :: fn (a: $T) => 1;
		foo :: fn (a: any) => 2;
		assert foo("hello" as any) == 2;
	}
	{
		foo :: fn (a: string) => 0;
		foo :: fn (a: ..string) => 1;
		assert foo("hello") == 0;
		assert foo() == 1;
		assert foo("a", "b") == 1;
	}
}
