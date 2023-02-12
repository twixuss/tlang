// PROGRAM OUTPUT Hello42[1, 2, 3]{x=4, y=5, z=6}

import "tlang/std.tl"

main :: () {
    print("Hello")
    print(42)
    print(.[1,2,3])
    print(Vector3(Int)(4,5,6))
}