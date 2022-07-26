import "std.tl"
main :: () {
    {
        for i in 5..10
            println(i)
    }
    println("")
    {
        arr: [3]Int
        arr[0] = 314
        arr[1] = 42
        arr[2] = 69
        for i in arr {
            println(i)
        }  
    }
    println("")
    {
        arr: [3]Int
        arr[0] = 1
        arr[1] = 4
        arr[2] = 9
        span: []Int = arr
        for i in span {
            println(i)
        }    
    }
}