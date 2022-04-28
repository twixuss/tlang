import "std.tl"

fac :: fn (v: int): int {
    return if v == 1 then 1 else v*fac(v-1);
}

main :: fn() {
    print(fac(5));
}
