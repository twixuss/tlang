import "std.tl"

typename :: fn (x) {
    T :: #typeof x;
    #if T == int return "int";
    #if T == string return "string";
    #if T == float return "float";
}

max1 :: fn(a: $T, b: T) => if a < b then b else a;
max2 :: fn(a, b: #typeof a) => if a < b then b else a;

main :: fn() {
    print(typename(12));
    print(typename("asd"));
    print(typename(1.2));
    print(max1(12, 32));
    print(max2(212, 132));
}
