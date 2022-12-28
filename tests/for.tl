import "tlang/std.tl"
main :: () {
    {
        for i in 5..10
            print("{} ", i)
    }
    println()
    {
        arr: [3]Int
        arr[0] = 314
        arr[1] = 42
        arr[2] = 69
        for i in arr {
            print("{} ", i)
        }
    }
    println()
    {
        arr: [3]Int
        arr[0] = 1
        arr[1] = 4
        arr[2] = 9
        span: []Int = arr
        for i in span {
            print("{} ", i)
        }
    }
    println()
    {
        x: [3]Int
        i := 0

        for * it in x {
            i += 1
            *it = i
        }

        println(x)

        y: []Int = x

        for * it in y {
            i += 1
            *it = i
        }

        println(y)
    }

    0
}