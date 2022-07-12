
import "std.tl"
 
overloaded_poly2 :: () {}
overloaded_poly2 :: (a: $T, b: int) {
    print("overloaded_poly2: ");
    print(a);
    print_char(' ');
    print(b);
    print_char('\n');
}

double_poly :: (a: $T, b: T, c: $U) {
    print("double_poly: ");
    print(a);
    print_char(' ');
    print(b);
    print_char(' ');
    print(c);
    print_char('\n');
}

test :: (a: int) {}
test :: (b: int) {}

Thing :: struct {
    do :: (this, a:    int) { print("Thing.do: "); print(a); print_char('\n'); }
    do :: (this, a: string) { print("Thing.do: "); print(a); print_char('\n'); }
}
main :: (): int {
    #print #function;
    {
        print("1:\n");
        f :: () {
            print("f()\n");
        }
        f();
        #assert !#compiles { f(1); };
        #assert !#compiles { f("2"); };
    }
    {
        print("2:\n");
        f :: (a: $T) {
            print("f($T): ");
            print(a);
            print("\n");
        }
        f(1);
        f("x");
        f(a=2);
        f(a="y");
    }
    {
        print("3:\n");
        f :: (a: $T, b: int) {
            print("f($T, int): ");
            print(a);
            print(" ");
            print(b);
            print("\n");
        }
        f(1, 2);
        f("x", 3);
        f(b=4, 5);
        f(b=6, "y");
    }
    {
        print("4:\n");
        f :: (a: $T, b: T) {
            print("f($T, T): ");
            print(a);
            print_char(' ');
            print(b);
            print_char('\n');
        }
        f(1, 2);
        f("x", "y");
        f(b=4, 3);
        f(b="w", "z");
    }
    {
        print("5:\n");
        f :: (a: int) {
            print("f(int): ");
            print(a);
            print_char('\n');
        }
        f :: (a: $T) {
            print("f($T): ");
            print(a);
            print_char('\n');
        }
        f(1);
        f("x");
        f(a=2);
        f(a="y");
    }
    {
        print("6:\n");
        f :: (a: ..int) {
            print("f(..int): ");
            i := 0;
            while i != a.count {
                print(a[i]);
                print_char(' ');
                i += 1;
            }
            print_char('\n');
        }
        f();
        f(1);
        f(2, 3);
    }
    {
        print("7:\n");
        f :: (a: string, b: ..int, c: string, d: ..int, e: string) {
            print("f(string, ..int, string, ..int, string): ");
            print(a);
            print_char(' ');
            i := 0;
            while i != b.count {
                print(b[i]);
                print_char(' ');
                i += 1;
            }
            print(c);
            print_char(' ');
            i = 0;
            while i != d.count {
                print(d[i]);
                print_char(' ');
                i += 1;
            }
            print(e);
            print_char('\n');
        }
        f("x", "y", "z");
        f("x", 1, "y", "z");
        f("x", "y", 1, "z");
        f("x", 1, "y", 2, "z");
        f("x", 1, 2, "y", 3, 4, "z");
    }
    {
        print("8:\n");
        f :: (a: ..string, b: ..int) {
            print("f(..string, ..int): ");
            i := 0;
            while i != a.count {
                print(a[i]);
                print_char(' ');
                i += 1;
            }
            i = 0;
            while i != b.count {
                print(b[i]);
                print_char(' ');
                i += 1;
            }
            print_char('\n');
        }
        f();
        f(1);
        f(1, 2);
        f("x");
        f("x", 1);
        f("x", 1, 2);
        f("x", "y", 1);
        f("x", "y", 1, 2);
    }
    {
        print("9:\n");
        f :: (a: ..$T) {
            print("f(..$T): ");
            i := 0;
            while i != a.count {
                print(a[i]);
                print_char(' ');
                i += 1;
            }
            print_char('\n');
        }
        // f();
        f(1);
        f(2, 3);
        f("x");
        f("x", "y");
    }


    test(a=10);    overloaded_poly2(3, 4);
    overloaded_poly2("5", 6);
    overloaded_poly2(b=4, 3);
    overloaded_poly2(b=6, "5");
    
    thing: Thing;
    thing.do(1);
    thing.do("2");
}
