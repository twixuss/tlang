main :: () {
	{
		foo :: (a: string) => 0;
		foo :: (a: $T) => 1;
		foo :: (a: any) => 2;
		assert foo("hello") == 0;
	}
	{
		foo :: (a: $T) => 1;
		foo :: (a: any) => 2;
		assert foo("hello") == 1;
	}
	{
		foo :: (a: string) => 0;
		foo :: (a: $T) => 1;
		foo :: (a: any) => 2;
		assert foo("hello" as any) == 2;
	}
	{
		foo :: (a: string) => 0;
		foo :: (a: ..string) => 1;
		assert foo("hello") == 0;
		assert foo() == 1;
		assert foo("a", "b") == 1;
	}
}
