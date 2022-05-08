# tlang
## Mini tutorial
[Skip](#types)
### Hello world!
```rs
import "std.tl"
main :: fn () {
    print("Hello world!\n");
}
```
Let's start with simple things, for example variable definition.
If you want to define a variable, use this syntax:
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
Ok, variables are covered, what about functions?
In this language almost everything is an expression. So are functions.
Here is a function expression:
```rs
fn (a: float, b: int): *void { return null; }
```
That was a function that takes a float an an int as parameters, and returns a pointer to void. Yes, there are pointers and they are written on the left of the type, as opposed to C. I want types to be easily readable from left to right, so that's why this is the way it is. Ok, let's go back to functions. How do you give it a name? Extremely easily:
```rs
do_stuff :: fn (a: float, b: int): *void { return null; }
```
So that's the basics of functions. I will not list all the features here to keep it simple. If you want to learn about them, you can find them in the [Functions features section](#functions). Now let's get to structures:
```rs
string :: struct {
  data: *u8;
  count: uint;
}
 // note that string literals are not null terminated. you have to write them explicitly using \0. maybe i will change this later.
```
This is literally a string type. Just a pointer and size. Like an array. Speaking of arrays, this is how you declare and use them:
```rs
array: [10]bool;
array[4] = true;
print("length of array is ");
print(array.count); // no formatting in standard library. sadge.
```
I should mention that all variables are initialized to zero by default.
Ok, enough with data, let's deal with control flow:
```rs
// if statement
if condition {
  do_stuff();
} else // braces are optional
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
#extern_library "kernel32.lib" // add this library to the list of libraries, in which all functions without a body will be searched for.
OpenFile :: fn #stdcall (...): ...;
```
Windows uses stdcall convention, so #stdcall directive says to use stdcall calling convention for this function. if you want to add a lot more functions with this convention, you may not write #stdcall for every function. Instead you can write the following statement:
```rs
#stdcall
```
This directive says to use stdcall calling convention for ALL following functions. if you want to restore language default calling convention, use #tlangcall directive.
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
```rs
span: []int;
array: [5]int;
span = array;
// NOTE: when converting from an array to a span, if the array is not addressable (for example was returned from a function), the array is temporarily stored in a stack memory, which is cleared at the beginning of each statement. This allows to pass arrays to functions that accept spans. Be careful: do not store spans of temporaries and then try to access it in another statement.
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
## Functions templates
Function template can be used to create multiple functions with different parameter types or values. You can create a template by making parameter's type polymorphic. A type is polymorphic if it contains `$` followed by a name.
```rs
poly :: fn (x: $T) {
  #print T; // prints at compile time
}
poly(123);      // prints int
poly("Hello "); // prints string
poly("World!"); // doesn't print anything because poly with string parameter is already instantiated and typechecked.
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
ptr : * #type fn (): int; // the same syntax is used for types. Because of that it's ambiguous if it's a type or a lambda without a body. So to distinguish between them we have to use #type directive.
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
```rs
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
# Features
* Compiled
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
* Foreign function interface
```rs
// Definitions of windows' functions
#extern_library "kernel32.lib"
#stdcall
ExitProcess :: fn (ret: u32);
```
* Source location
```rs
print(#file); // source.tl
print(#line); // 2
print(#location); // source.tl:3:14
```
* Module importing
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