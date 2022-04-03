import "std.tl"

main :: fn() {
    defer print("A\n");
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
    print("Hello World!\n");
}

