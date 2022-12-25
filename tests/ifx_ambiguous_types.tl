// COMPILER ERROR
// COMPILER OUTPUT Branches of this if expression have different types, and both are implicitly castable to each other, which makes it ambiguous

Foo :: struct {}
Bar :: struct {}

operator as implicit :: (x: Foo) => Bar()
operator as implicit :: (x: Bar) => Foo()

main :: () {
    return if false then Foo() else Bar()
}
