import "std.tl"

main :: fn () {
    c : [5]string;
    
    i : int = 0;
    while i < 5 {
        [i]c.data = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA".data;
        [i]c.count = (i+1) * (i+1);
        i = i + 1;
    }

    a[i].b[j];

    [j][i]a.b

    i : int = 0;
    while i < 5 {
        print([i]c);
        print("\n");
    }
}
