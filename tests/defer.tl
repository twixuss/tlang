import "tlang/std.tl"

main :: () {
    defer { println("A") }
    defer println("B")

    {
        {
            defer println("C")
        }
        defer println("D")
        return
        defer println("E")
    }

    defer println("F")
    println("G")
}

