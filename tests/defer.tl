import "std.tl"

main :: () {
    defer { print("A\n"); }
    defer print("B\n");

    {
        {
            defer print("C\n");
        }
        defer print("D\n");
        return;
        defer print("E\n");
    }
    
    defer print("F\n");
    print("G\n");
}

