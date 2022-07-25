
# tlang
Inspired by jai, odin, zig and other languages.
### Hello world!
```java
import "std.tl"
main :: () {
    println("Hello world!");
}
```
# Language
## Definitions
```java
name := expression; // variable
name :: expression; // constant
```
This way the type of a variable will be inferred automatically.
If you want to explicitly specify the type, put it after first colon, like this:
```java
my_counter: Int = 42;
```
Note that spaces here are not significant, so these are the same statements:
```java
abc:String="Hello!";
abc
:    String =   "Hello!";
```
## Blocks
Block is a statement that contains a list of other statements.
It is the same thing as in many other languages. 
```java
a := 1;
{
  b := 2;
}
```
## Functions
Like in most other languages, a function is a block of code which you can call on demand, that takes some parameters and returns a value.
In this language a function is an expression:
```java
(a: Float, b: Int): *Void { return null; }
```
Function can be named like any other thing:
```java
do_nothing :: (a: Float, b: Int): *Void { return null; }
```
You can use named parameters:
```java
function :: (a: Int, b: Int) {...}
function(b=12, a=99);
```
Functions can be overloaded on a parameter *type*:
```java
print :: (value: Int) {...}
print :: (value: String) {...}

print(1);   // calls the first one
print("2"); // calls the second one
```
Also functions can be overloaded on a parameter *name*:
```java
print :: (a: Int) {...}
print :: (b: Int) {...}

print(a=1); // calls the first one
print(b=2); // calls the second one
// print(3); // does not compile - ambiguous.
```
## Structs
A structure is also an expression. You can use it like this:
```java
// This is a String type as it is in the language.
String :: struct {
  data: *U8;
  count: UInt;
}
```
Note that string literals are null terminated, but the terminator is not included in the count.
## Arrays
Array types are made like this:
```java
[count]type
```
Indexing an array:
```java
array[index]
```
```java
array: [10]Int;
array[4] = 42;
print("length of array is ");
println(array.count);
print("pointer to first element is ");
println(array.data);
```
## Defer
```java
// this statement will be executed at the end of the scope.
defer print("deferred\n");
print("first\n");
```
`defer` statement will delay the execution of following statement until the end of scope. So this code will print "first", then "deferred".
# Control flow
```java
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

x := 1;
match x {
  0 => println("good");
  1 => println("ok");
  2 => println("bad");
  else => println("???");
}
// Only constant integers are supported right now, including enums.
// Matching an enum without default case is checked to cover all possible values.
// Maybe I'll add a way to match with default case and the checks.
```
# External functions
```java
import "windows.tl"
```
`windows.tl` is a file in `libs` directory of the compiler. It defines basic win32 functions, like printing to console or creating a window. If it doesn't have one that you need, you can always add it yourself:
```java
#extern_library "kernel32.lib" 
OpenFile :: (...): ... #stdcall;
// functions without a body will be searched for in extern libraries.
```
Windows uses stdcall convention, so #stdcall directive says to use stdcall calling convention for this function. if you want to add a lot more functions with this convention, you may not write #stdcall for every function. Instead you can write the following statement:
```c
#stdcall
```
This directive says to use stdcall calling convention for ALL following functions. if you want to restore language default calling convention, use `#tlangcall` directive.
## "Macros"
```c
#line           // line number
#column         // column number
#file           // file string
#function       // function type string *
#assert         // compile-time assert
#print expr     // compile-time print
#typeof expr    // returns the type of expr
#compiles {...} // evaluates to true or false depending if the following block compiles or not.
                // that block will not be evaluated at runtime.
```
\* Because of return type deduction, if no return type was specified, #function has to be
evaluated after typechecking the entire body. It will evaluate to correct name at runtime, but if
it is used in constant context, it will result in "undefined".
# Types
## Optional
```java
func :: (a: ?String) {
  print(if a then *a else "a is empty");
}
```
## Array
```java
array: [10]Int;
array[0] = 12;
// pointer to first element is array.data
// number of elements is array.count
```
## Span
Span is just a pointer to an array and the number of elements. 
Span does not have to cover an entire array, it can point at a portion of it.
```java
span: []Int;
{
  array: [4]Int;
  span = array;
  // now span covers the entire array.
  
  span[0] = 14;
  print(array[0]); // prints 14

  span.data = &array[1];
  span.count = 2;
  // now span includes only elements [1] and [2].
  
  span[0] = 12;
  print(array[1]); // prints 12
}
// array is gone after end of scope, so span is pointing at freed memory,
// which will be used for next variables.
```
There is an implicit conversion from an array to a span.
```java
process :: (span: []Int);
get_array :: (): [4]Int;

process(get_array());

// In this example we need to convert an array to a span, but we can't
// get the address of the array because it is returned from a function.
// To allow this conversion, the array is temporarily stored in a reserved stack memory.
// This memory region can be overwritten by subsequent statements.
```
# Functions
## Return type deduction
```java
get_string :: () { // no need to put `:String` in here
  return "what's up";
}
```
## Named return parameters
```java
test :: (): named_return_value: Int {
  named_return_value = 42; // this is the same as return 42;
}
```
They can be used when creating objects:
```java
create :: (): result: StringBuilder {
  result.last = &result.first;
  result.alloc_last = &result.first;
}
```
Also named return parameters can be useful when you need to do something with the value right before returning from the function:
```java
process :: (data: Data): result: ProcessedData  {
  defer do_something(result);
  return if condition then process1(data) else process2(data);
}
```
## Functions always initialize the return value
```java
forgot_to_return :: (): Int {
  // oops. no return statement! no worries. this will always return 0.

  // I don't know if compiler should always warn you if you forget to explicitly return.
  // For example when using named return parameters, this warning becomes kinda annoying.
  // For now it will issue a warning only for unnamed return parameters.
}
```
## Packs
Packs allow you to pass variable amount of arguments of the same type to a function.
```java
vararg :: (pack: ..String) {
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
vararg :: (first: Int, middle: ..String, last: Int) {...}

vararg(1, 2);
vararg(1, "hello", 2);
vararg(1, "hello", "world", 2);
```
You can have multiple packs in a function.
```java
vararg :: (a: ..Int, b: ..String) {...}

vararg(1, 2);
vararg(1, "hello");
vararg("hello", "world");
```
An argument will be put into a pack while it implicitly converts to the pack's element type.
This means it is not possible to have subsequent packs of the same type.

If you want to pass a pack to a function that accepts another pack, you have to use `..` syntax:
```java
foo :: (a: ..Int) {...}
bar :: (b: ..Int) => foo(..b);
```
You might think that this is redundant, because you just passing a span to a function that accepts the same span, and you will be right in that case. This will not work with `Any`. Consider this example:
```java
foo :: (a: ..Any) {...}
bar1 :: (b: ..Any) => foo(..b);
bar2 :: (b: ..Any) => foo(b);
```
`bar1` will call foo with the same received arguments.
`bar2` howewer will call foo with just one argument of type `[]any`.
## Functions templates
Function template can be used to create multiple functions with different parameter types or values. You can create a template by making parameter's type polymorphic. A type is polymorphic if it contains `$` followed by a name.
```java
poly :: (x: $T) {
  #print T; // prints type name at compile time
}
poly(123);      // prints Int
poly("Hello "); // prints String
poly("World!"); // doesn't print anything because poly with String
                // parameter is already instantiated and typechecked.
```
## Function templates with packs
You can make a pack be a template.
```java
poly :: (x: ..$T) {}

// poly(); // error: could not determine $T type
poly(1, 2);
poly("x", "y");
```
## Constant parameters
You can force the parameter to be constant by using % operator:
```java
get :: (value: %Int) {
  arr: [value]String;
  return value * 7;
}
```

To call this function, you'll have to pass a constant to it.
```java
get(12); // ok
x := 12;
get(x); // error, x is not constant.
```
This is a kind of function template. For every unique constant you pass in, a copy of the template will be instantiated. This can be used not only with values, but also types:
```java
new :: (T: %Type): *T {
  return malloc(#sizeof T) as *T;
}
```
## A bit of syntactic sugar
```java
sweet :: () => "YEP";
// this is equivalent to
sweet :: () { return "YEP"; }
```
## A pointer to a function
```java
ptr : * #type (): Int;
// the same syntax is used for types. Because of that it's
// ambiguous if it's a type or a lambda without a body.
// So to distinguish between them we have to use #type directive.
```
## Static functions
```java
Thing :: struct {
  static_function :: (...): ... {
    ...
  }
}

something := Thing.static_function(...);
```
Static functions can be inserted into a struct from outside.
```java
create :: (this :: Thing, param: Int): Thing { ... }
```
## Member functions
```java
Thing :: struct {
  member_function :: (this, ...): ... {
    ...
  }
}

thing: Thing;
something := thing.member_function(...);
```
### TODO
Like static functions, member functions also can be inserted into a struct from outside.
```java
create :: (this: Thing, param: Int): Thing { ... } // Note only single colon after `this`
```
# Temporary space
This language provides a lot of implicit conversions to simplify things, 
for example from array to span, or from any type to `any` type, etc.
These conversions require taking a pointer to something, which is fine if
that something is for example a stack variable, just take it's address.
However, what if conversion from a non-addressable value happens?
```java
foo :: (x: any) { ... }

get_thing :: (): [1024]Int { ... }

foo(get_thing());
```
In this case, you can't just take an address of `get_thing()`.
For that case compiler puts the value returned by `get_thing()` in the temporary region of the stack
and passes the pointer to `any`. That temporary region is untouched until the scope is closed. 
After that that temporary region may be overwritten by subsequent users of the temporary space.

This makes doing something like this possible without much trouble:
```java
x: any = 2; // that 2 is stored until the scope closes.
```

### TODO
Compare different temporary space sizing strategies.

Right now the size of temporary space is determined per-scope (maximum of all scopes in a function).
This method may use more memory than in per-statement case, but it makes things easier.
Maybe there's a better strategy for determining temporary space size.
Maybe "bind" a temporary allocation to a definition that "invoked" it an subtract the size after the last use of that definition?
I don't know if memory savings are worth the complication.

# Autocast
You can easily cast an expression to a type known by compiler by using `@` operator:
```java
a: Int;
b: *Void;
// a = b;     // error, *Void is not implicitly convertible to *Int.
a = b as Int; // can explicitly specify the type.
a = @b;       // same as previous, but the type is inferred.
```
If @ operator is used in a context where destination type is not known, it is ignored.
# Type information
You can get information about any type at compile time from `#typeinfo` expression
```java
info := #typeinfo Int;
println(info);
```
This information contains most of the things you need to know about the type like size, members and a lot more.

Type information is so simple yet so useful.
```java
Vector2 :: struct {
  x: Float32;
  y: Float32;
}

println(Vector2(1, 2));
```
Yes, you don't have to print each member manually, everything is printable by default. 
# Other things
* All variables are initialized to zero by default.
* Compile time execution (only basic operations are supported for now)
* Global definitions can be ordered in any way
```java
main :: () => print(X);
X :: 1337;
```
* Literals are unlimited in size. They will be converted to required type at use site. Compiler will warn you if the value does not fit into destination type.
```java
a :: 999999999999999999999999999 * 123456789;
```
* Lossy conversions are always explicit.
* Nested comments `/* /* /* */ */ */`
# TODO
* [ ] Builtin types
  * [ ] variant
* [ ] SIMD
* [ ] Multiple return parameters
* [ ] Optimization
* [ ] Context
* [ ] Extension methods (WIP)
* [ ] Specify build options in the source code
* [ ] Default arguments
* [ ] AST inspection.
* [ ] Caller argument expression string
```java
func :: (a: String) {
  #print #exprof a;
}

func(get_the_string(12, 34))

// this should print "get_the_string(12, 34)"
```