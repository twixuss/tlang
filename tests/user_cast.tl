import "std.tl"

stringize :: fn (v: int): string {
    return if v & 1 == 0 then "even" else "odd";
}

Thing :: struct {
    str: string;
}

operator as implicit :: fn (v: int): Thing {
    result: Thing;
    result.str = stringize(v);
    return result;
}

main :: fn() {
    thing: Thing = 15 as int;
    print(thing.str);
}
