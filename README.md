# tlang
My experimental programming language

This is an imperative language i develop for fun.

## Hello world!
```rs
import "std.tl"
main :: fn () {
    print_string("Hello world!\n");
}
```

## Current features:
* Compiled
* Compile time execution (only basic operations are supported for now)
* Compile time string printing
```rs
#print #typeof(3.4 + 6.5) // prints 'float'
```
* Compile time assertions
* Global definitions can be ordered in any way
```rs
main :: fn () => print_int(X);
X :: 1337;
```
* Almost everyting is an expression
```rs
print_string(if true then "Hello" else "World");
```
* Types are also expressions
```rs
my_int :: int;
value : my_int = 42;
```
* Foreign function interface
```rs
// Definitions of windows' functions
#extern_library "kernel32.lib"
#stdcall
ExitProcess :: fn (ret: u32);
```
* Source location constants
```rs
print_string(#file); // a.tl
print_int   (#line); // 2
print_string(#location); // a.tl:3:14
```
* Module importing
* Function overloading
```rs
import "std.tl"
foo :: fn (a: int) => print_string("int");
foo :: fn (a: bool) => print_string("bool");
main :: fn () {
  foo(true);
}
```
* Literals are unlimited in size
```rs
a :: 999999999999999999999999999 * 123456789;
```
* Implicit lossless conversions
* Nested comments `/* /* /* */ */ */`

## TODO
* Functions with polymorphic arguments
* Member functions
* Builtin types
  * optional
  * union
  * variant
* More useful standard library
* SIMD
* Multiple return parameters
* Optimization
* Context
* Extension methods
* Metaprogramming
* Specify build options in the source code
