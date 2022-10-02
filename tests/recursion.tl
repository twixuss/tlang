import "tlang/std.tl"

fac :: (v: Int) Int => if v == 1 then 1 else v * fac(v-1)

main :: () {
    print(fac(5))
}
