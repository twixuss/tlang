fac :: (v: Int) Int => if v == 1 then 1 else v * fac(v-1)

main :: () {
    assert fac(5) == 120
}
