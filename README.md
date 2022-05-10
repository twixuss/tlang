# tlang
Inspired by jai, odin, zig and other languages.
### Hello world!
```rs
import "std.tl"
main :: fn () {
    print("Hello world!\n");
}
```
# Statements
## Definitions
```rs
name := expression; // variable
name :: expression; // constant
```
In this case the type of a variable will be inferred automatically.
If you want to explicitly specify the type, put it after first colon, like this:
```rs
my_counter : int = 42;
```
Note that spaces here are not significant, so these are the same statements:
```rs
abc:string="Hello!";
abc
:    string =   "Hello!";
```
# Expressions
## Functions
In this language a function is an expression:
```rs
fn (a: float, b: int): *void { return null; }
```
That was a function that takes a float and an int as parameters, and returns a pointer to void. Define a function like you would a variable:
```rs
do_stuff :: fn (a: float, b: int): *void { return null; }
```
You can use named parameters:
```rs
function :: fn (a: int, b: int) {...}
function(b=12, a=99);
```
Functions can be overloaded on a parameter type:
```rs
print :: fn (value: int) {...}
print :: fn (value: string) {...}

print(1);   // calls the first one
print("2"); // calls the second one
```
Also functions can be overloaded on a parameter name:
```rs
print :: fn (a: int) {...}
print :: fn (b: int) {...}

print(a=1); // calls the first one
print(b=2); // calls the second one
// print(3); // does not compile - ambiguous.
```
## Structs
A structure is also an expression. You can use it like this:
```rs
string :: struct {
  data: *u8;
  count: uint;
}
// note that string literals are not null terminated.
// you have to write them explicitly using \0. maybe i will change this later.
```
This is a string type as it is in the language.
## Arrays
Array types are made like this:
```
[count]type
```
Indexing an array:
```rs
array[index]
```
```rs
array: [10]bool;
array[4] = true;
print("length of array is ");
print(array.count);
```
# Control flow
```rs
// if statement
if condition {
  do_stuff();
} else // braces are optional for single statement
  do_other_stuff();

// if expression
value := if condition then 42 else 1337;

// while loop
while condition {
   do_stuff();
}

// the same thing, just to demonstrate
while condition do_stuff();
```
These are all control flow constructs in the language. There is one more thing I want to cover before going to modules:
```rs
// this statement will be executed at the end of the scope.
defer print("deferred\n");
print("first\n");
```
`defer` statement will delay the execution of following statement until the end of scope. So this code will print "first", then "deferred". Let's look at a more sophisticated example:
```rs
{
  defer print("A");
  defer print("B");
  {
    defer print("C");
  } // scope 1
  defer print("D");
  return;
  defer print("E");
} // scope 2
```
This code will print "CDBA". Do you know what is a stack? Imagine that each scope (these curly braces enclose a scope) has a stack of code. When execution "goes through" a line with defer statement, it's code is pushed to defer's parent scope. So `print("A")` will be pushed to second scope's stack. Now, when it's time to leave the scope, the compiler will just grab the code from the stack and execute it, until there's nothing left on the stack.

Alright, now we can start writing simple programs. Let's do something a bit more compicated. What about talking to the operating system? To do this, you can import your operating systems' function signatures by writing this:
```rs
import "windows.tl"
```
`windows.tl` is a file in `libs` directory of the compiler. It defines basic win32 functions, like printing to console or creating a window. If it doesn't have one that you need, you can always add it yourself:
```rs
#extern_library "kernel32.lib" 
OpenFile :: fn #stdcall (...): ...;
// functions without a body will be searched for in extern libraries.
```
Windows uses stdcall convention, so #stdcall directive says to use stdcall calling convention for this function. if you want to add a lot more functions with this convention, you may not write #stdcall for every function. Instead you can write the following statement:
```c
#stdcall
```
This directive says to use stdcall calling convention for ALL following functions. if you want to restore language default calling convention, use #tlangcall directive.
## "Macros"
```c
#line           // line number
#column         // column number
#file           // file string
#function       // function type string *
#assert         // compile-time assert
#print          // compile-time print
#compiles {...} // evaluates to true or false depending if the following block compiles or not.
                // that block will not be evaluated at runtime.
```
\* Because of return type deduction, if no return type was specified, #function has to be
evaluated after typechecking the entire body. It will do what you expect at runtime, but if
it is used in constant context, it will result in "undefined".
# Types
## Optional
```rs
func :: fn (a: ?string) {
  print(if a then *a else "a is empty");
}
```
## Array
```rs
array: [10]int;
array[0] = 12;
// pointer to first element is array.data
// number of elements is array.count
```
## Span
You can think of a span as just a pointer to an array, except that it also stores the number of elements.
Span does not have to cover an entire array, it can point at a portion of it.
```rs
span: []int;
{
  array: [5]int;
  span = array;
  // now span covers the entire array.
  
  span[0] = 14;
  print(array[0]); // prints 14

  span.data = &array[1];
  span.count = 3;
  // now span misses first and last elements.
  
  span[0] = 12;
  print(array[1]); // prints 12
}
// array is gone after end of scope, so span is pointing at freed memory,
// which will be used for next variables.
```
There is an implicit conversion from an array to a span.
```java
process :: fn (span: []int);
get_array :: fn (): [4]int;

process(get_array());

// In this example we need to convert an array to a span, but we can't
// get the address of the array because it is returned from a function.
// To allow this conversion, the array is temporarily stored in a reserved stack memory.
// This memory region can be overwritten by subsequent statements.

// Don't do this:
span: []int = get_array();
// span does not "own" the memory, it is already pointing at garbage here.
```
# Functions
## Return type deduction
```rs
get_string :: () { // no need to put `:string` in here
  return "what's up";
}
```
## Named return parameters
```rs
test :: fn (): named_return_value: int {
  named_return_value = 42; // this is the same as return 42;
}
```
They can be used when creating objects:
```rs
create :: fn (): result: StringBuilder {
  result.last = &result.first;
  result.alloc_last = &result.first;
}
```
Also named return parameters can be useful when you need to do something with the value right before returning from the function:
```rs
process :: (data: Data): result: ProcessedData  {
  defer do_something(result);
  return if condition then process1(data) else process2(data);
}
```
## Functions always initialize the return value
```rs
forgot_to_return :: fn (): int {
  // oops. no return statement! no worries. this will always return 0.

  // I don't know if compiler should always warn you if you forget to explicitly return.
  // For example when using named return parameters, this warning becomes kinda annoying.
  // For now it will issue a warning only for unnamed return parameters.
}
```
## Packs
Packs allow you to pass variable amount of arguments of the same type to a function.
```java
vararg :: fn (pack: ..string) {
  i := 0;
  while i != pack.count {
    print(pack[i]);
    i += 1;
  }
}

vararg();
vararg("hello");
vararg("hello", "world");
```
Basically a pack is just a span. When you pass multiple arguments to a pack, a temporary array
with specified values is created, and a span to it is passed to the function.

Parameter pack does not have to be last in the argument list.
```java
vararg :: fn (first: int, middle: ..string, last: int) {...}

vararg(1, 2);
vararg(1, "hello", 2);
vararg(1, "hello", "world", 2);
```
You can have multiple packs in a function.
```java
vararg :: fn (a: ..int, b: ..string) {...}

vararg(1, 2);
vararg(1, "hello");
vararg("hello", "world");
```
An argument will be put into a pack while it implicitly converts to the pack's element type.
This means it is not possible to have subsequent packs of the same type.
## Functions templates
Function template can be used to create multiple functions with different parameter types or values. You can create a template by making parameter's type polymorphic. A type is polymorphic if it contains `$` followed by a name.
```rs
poly :: fn (x: $T) {
  #print T; // prints type name at compile time
}
poly(123);      // prints int
poly("Hello "); // prints string
poly("World!"); // doesn't print anything because poly with string
                // parameter is already instantiated and typechecked.
```
If you don't need a type name, just omit the type completely:
```rs
poly :: fn (x) {...}
```
## Constant parameters
You can force the parameter to be constant by using % operator:
```rs
get :: fn (value: %int) {
  return value * 7;
}
```
That way you can use `value` in places that require a constant, like array sizes.

To call this function, you'll have to pass a constant to it.
```rs
get(12); // ok
x := 12;
get(x); // error, x is not constant.
```
This is a kind of function template. For every unique constant you pass in, a copy of the template will be instantiated. This can be used not only with values, but also types:
```rs
new :: (T: %type): *T {
  return malloc(#sizeof T) as *T;
}
```
## A bit of syntactic sugar
```rs
sweet :: fn () => "YEP";
// this is equivalent to
sweet :: fn () { return "YEP"; }
```
## A pointer to a function
```rs
ptr : * #type fn (): int;
// the same syntax is used for types. Because of that it's
// ambiguous if it's a type or a lambda without a body.
// So to distinguish between them we have to use #type directive.
```
## Static functions
```rs
Thing :: struct {
  static_function :: fn (...): ... {
    ...
  }
}

something := Thing.static_function(...);
```
## Member functions
```java
Thing :: struct {
  member_function :: fn (this, ...): ... {
    ...
  }
}

thing: Thing;
something := thing.member_function(...);
```
# Autocast
You can easily cast an expression to a known type by using @ operator:
```rs
a: int;
b: *void;
// a = b;     // error, *void is not implicitly convertible to int.
a = b as int; // have to explicitly specify the type.
a = @b;       // same as previous, but the type is inferred.
```
Because type is inferred from the destination, this operator can be used only in specific places:
 * definitions with explicit type: `x: int = @y`
 * assignments: `x = @y`
 * returns (when return type is not deduced): `return @y`
 * calls: `x(@y)`

If @ operator is used in a context where destination type is not known, it is ignored.
# Other things
* All variables are initialized to zero by default.
* Compile time execution (only basic operations are supported for now)
* Compile time string printing
```rs
#print #typeof(3.4 + 6.5) // prints 'float'
```
* Compile time assertions
* Global definitions can be ordered in any way
```rs
main :: fn () => print(X);
X :: 1337;
```
* Source location
```rs
print(#file); // source.tl
print(#line); // 2
print(#location); // source.tl:3:14
```
* Function overloading
```rs
import "std.tl"
foo :: fn (a: int) => print("int");
foo :: fn (a: bool) => print("bool");
main :: fn () {
  foo(true); // prints bool
}
```
* Literals are unlimited in size
```rs
a :: 999999999999999999999999999 * 123456789;
```
* Implicit lossless conversions
* Nested comments `/* /* /* */ */ */`
## TODO
* Builtin types
  * variant
* More useful standard library
* SIMD
* Multiple return parameters
* Optimization
* Context
* Extension methods
* Metaprogramming
* Specify build options in the source code
* Default arguments
* Ability to specify the time at which an expression will be evaluated (compile time / runtime)
* Caller argument expression string
```rs
func :: fn (a: string) {
  #print #exprof a;
}

func(get_the_string(12, 34))

// this should print "get_the_string(12, 34)"
```
* Deferred argument evaluation
```rs
func :: fn (defer a: string) {
  if get_condition() print(a);
}

func(get_string());
// get_string is called only if get_condition returned true
```