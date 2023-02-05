
# tlang
tlang is a compiled language, intended to be simple, expressive and easy to use.

Inspired by jai, odin, zig, nim and other languages.
# Hello world!
```java
import "std.tl"
main :: () {
    println("Hello world!")
}
```
# Comments
Comments in this language are like in C.
```java
// Single line comment

/*
Multiline
Comment
*/
```
The only thing that's different is that you can have nested multiline comments.
```java
/* This /* Does */ Work */
```
# Identifiers
An identifier is a name that references something.

Its first symbol can't be a number. Examples:
```java
x
hello_mister_123
♥
```
You can't use reserved keywords and words that start with a number as identifiers
```java
struct // `struct` is a keyword, not an identifier
123    // `123' is a literal
```
There is a way to work around that. You can prefix such word with `\` to make it an identifier:
```java
\struct // this is an identifier whose name is 'struct'
\123    // identifier '123'
```
Backslash is not included in the identifier name.

Also you can use `\` to merge two identifiers in a single one:
```java
hello_  \  world
hello_world
// These are the same
```
# Literals
Literal is a simplest expression. They are constant and always evaluate to the same value.
```java
1          // Int
1.2        // Float
"Hello"    // String
true false // Bool
'\n'       // U8          (There's no char type. ASCII)
null       // pointer
```
Numeric literals in this language are unsized. You don't have to write suffixes to specify the type (e.g. 42ull in C++).

Strings in this language are represented with a `data` pointer and byte `count`.
Literals are null-terminated, but the terminator is not included in the `count`.
If the quotes of a string literal are located on different lines, the literal becomes multiline.
Such literal does not include the first and the last newline.
```java
"
Hello

"
// is equivalent to
"Hello\n"
```
## TODO: Indentation in multiline string literals
# Definitions
Definition is a statement that introduces a name, associated with some value, in the current scope.
```java
value: Int = 42 // This is a variable
```
To make this a constant, use ':' instead of '='
```java
value: Int : 42 // This is a contant
```
Note that unlike in other languages, constant means constant and not read-only, meaning it is always the same, therefore it must be known at compile time.

You don't have to always put the type after the name:
```java
value :: 42 // Compiler infers that the type is Int from the expression
```
# Blocks
Block is a statement that contains a list of other statements.
It is the same thing as in many other languages.
```java
a := 1
{
  b := 2
}
```
# Lambdas
## Basics
### Writing lambdas
Lambda is a block of code that accepts zero or more arguments and returns a value to the caller.
This is the simplest lambda:
```java
()
// This is a lambda that accepts zero arguments, returns nothing and does not have a body.
```

A body of a lambda is defined with curly braces. You can put other statements between them, like in blocks.
```java
() {
}
// This is still a lambda that accepts zero arguments and returns nothing.
```
To make the lambda accept an argument, just put the argument definition inside the brackets.
Multiple arguments are separated with a comma. To specify the return type of a lambda write a colon after closing bracket and then the type.
```java
(a: Int, b: Int) Int {
  // Now we can do something with "a" and "b"
  return a + b
}
```
Note that return type is actually a part of a definition that can be given a name:
```java
(a: Int, b: Int) result: Int {
  result = a + b
}
```
With named return parameters we don't have to use `return` statement, the result will be implicitly returned for us.

What happens if we don't assign to it you may ask? In that case the function will return the default value of the return type. For `Int` it is `0`.

Another question is what if we don't use named return parameter and don't return from a function?
The same thing happens: the default value is returned.

The last thing about return parameters is that you don't have to provide an expression for the return statement:
```java
() result: Int {
  result = 42
  return
}
```

Let's move on to ways of simplifying this lambda.

There are three ways how we can do that. First we can use parameter type only once:
```java
(a, b: Int): Int {
  return a + b
}
```
Then we can use '=>' syntax to make this thing even shorter:
```java
(a, b: Int): Int => a + b
```
And finally we can get rid of ": Int" because the compiler can infer the type from the return expression.
```java
(a, b: Int) => a + b
```
Now we can assign this lambda to a name for calling it later:
```java
add :: (a, b: Int) => a + b // Here `add` is a regular function

add := (a, b: Int) => a + b // Here `add` is a function pointer, that can later be reassigned.
```

### Using lambdas
To call a function `add` which we wrote in the previous chapter, we can use the following syntax:
```java
add(1, 2)
```
Looking at the order of arguments we can see that 1 goes into a and 2 goes into b.
When lambdas have a lot of arguments this may not be clear right away.
This language provides a way to say what is what:
```java
add(b=2, a=1)
// As you can see you can reorder arguments
```
Also this language allows you to call a function using different syntaxes:
```java
add(1, 2) // Free syntax
1.add(2)  // Member syntax
1 add 2   // Binary (infix) syntax (obviously only for functions with 2 arguments)
```
All of these are available by default.
## Uniform function call syntax
All functions you write are freestanding. If a function satisfies syntax's rules,
it is callable with that syntax by default, without any extra declarations.
### Member
Any free function with at least
one parameter is callable using member call syntax:
```java
fma :: (a, b, c: Int) => a * b + c

1.fma(2, 3) // a = 1, b = 2, c = 3
```
First parameter to such function can be a pointer. In that case,
if the expression before the dot is addressable, its address is
implicitly taken, otherwise the value is put on the stack and only then its address is taken:
```java
Vector2 :: struct { x, y: Float }
length :: (using v: Vector2) => sqrt(x*x + y*y)

normalized :: (v: Vector2) => v / v.length() // v is passed by value
                                             // and can not be modified.

normalize :: (v: *Vector2) { *v = v.normalized() } // v is passed by pointer
                                                   // and can be modified.

a := Vector2(1, 2)
a.normalize() // a is modified

println(Vector2(3, 4).normalize()) // `Vector2(3, 4)` is not addressable,
                                   // so it is put on the stack
```
### Infix
Any free function with exactly
two parameters is callable using infix syntax:
```java
add :: (a, b: Int) => a + b

println(1 add 2)
```
How this operator behaves with other operators is described in [Operator precedence](#operator-precedence) section

Note: I think this feature is not worth it, because it makes error messages terrible
if you for example forgot `then` keyword in an `if` statement.
## Default arguments
Any parameter in a lambda can be assigned a default value. If an argument was not provided
for that parameter when calling this function, the default will be used.
```java
foo :: (a: Int = 5) => println(a)

foo()   // prints 5
foo(42) // prints 42
```
## Variadics
There is a way to say that a lambda accepts an unknown number of arguments:
```java
foo :: (ints: ..Int)
// You can call this function with any number of ints:
foo()
foo(1)
foo(1, 2, 3)
```
In terms of usage `..Int` is the same as `[]Int`. You can get the `data`, `count`, iterate over it, do whatever you want.
This language does not put restrictions on where this kind of argument has to be:
```java
foo :: (ints: ..Int, valid: Bool, strings: ..String)

foo(true, "hello", "world")
```
There must be a clear boundary between variadics. If the types are implicitly convertible,
the compiler will not be able to disambiguate the argument order.
## External functions
```java
import "windows.tl"
```
`windows.tl` is a file in `libs` directory of the compiler. It defines basic win32 functions, like printing to console or creating a window. If it doesn't have one that you need, you can always add it yourself:
```java
#extern_library "kernel32.lib"
OpenFile :: (...) ... #stdcall
// functions without a body will be searched for in extern libraries.
```
Windows uses stdcall convention, so #stdcall directive says to use that calling
convention for this function. If you want to add a lot more functions with this
convention, you don't have to write #stdcall for every function. Instead you can write
the following statement:
```java
#stdcall
// define all stdcall functions
```
This directive says to use stdcall calling convention for ALL following functions. if you want to restore language default calling convention, use `#tlangcall` directive.
# Structs
A structure is a construct that allows you to group related data together:
```java
Cat :: struct {
  name: String
  age: Int
  lives_left: Int
}
```
You can create on of these like this:
```java
cat: Cat
cat.name = "Funny"
cat.age = 6
cat.lives_left = 9
```
Or like that:
```java
cat := Cat(name = "Funny", age = 6, lives_left = 9)
// NOTE: if no member names were specified,
// members are initialized in order of declaration.
```
Structs can have default values:
```java
Thing :: struct {
  value := 42
}

print(Thing().value) // prints 42
```
Because the above struct definitions are constant,
a structure may seem to be a literal, but it is not.
If you write two identical structs, they are considered different types.
# Arrays
Array types are made like this:
```java
[count]type
```
Indexing an array:
```java
array[index]
```
```java
array: [10]Int
array[4] = 42
print("length of array is ")
println(array.count)
print("pointer to first element is ")
println(array.data)
```
# Defer
```java
{
  // this statement will be executed at the end of the scope.
  defer println("deferred")
  println("first")
}
println("last");
```
`defer` statement will delay the execution of following statement until the end of scope.
So this code will print "first deferred last".
# Control flow
## If statement
```java
if condition {
  do_stuff()
} else // braces are optional for single statement
  do_other_stuff()
// NOTE: `if` oneliners without braces require using `then` keyword
```
In the above example `condition` will be evaluated at runtime. If you want to do it
at compile time, prefix `if` with a `#`:
```java
#if condition then do_stuff()
else do_other_stuff()
```
Compiler has to evaluate `condition` at compile time, and depending on its value it
will only keep a single branch in the executable.
## If expression
```java
value := if condition then 42 else 1337
```
## While statement
```java
// while loop
while condition {
   do_stuff()
}
// NOTE: `while` oneliners without braces require using `do` keyword
```
## For statement
```java
// for loop
for it in expression {
  println(it)
}
// NOTE: `for` oneliners without braces require using `do` keyword

// iterable types are:
// spans  - []Int
// arrays - [5]Int
// ranges - 0..10
```
Iterate by pointer:
```java
for * it in expression {
  println("address: {}, value: {}", it, *it)
}
```
## Match statement
```java
x := 1
match x {
  0 => println("good")
  1 => println("ok")
  2 => println("bad")
  else => println("???")
}
```
Only constant integers and enums are supported right now.
Matching an enum without default case is going to throw a warning if not all cases are handled.
Maybe I'll add a way to match with default case and the checks.
# Templates
Templates are used to avoid code duplication.
A template is not a real lambda or struct, it is a 'recipe' of how to create one.

A struct template is made by providing a parameter list after `struct` keyword.

A lambda template is made by making its parameter constant, or by introducing a deducable type.
## Constant parameters
You can force a parameter to be constant by using `%`:
```java
func :: (value: %Int) {
  arr: [value]String
  // do something with arr
}
```

To call this function, you'll have to pass a constant to it.
```java
get(12) // ok
x := 12
get(x) // error, x is not constant.
```
For every unique constant you pass in, a copy of the template will be instantiated. This can be used not only with values, but also types:
```java
new :: (T: %Type): *T {
  return malloc(#sizeof T) as *T
}

a := new(Int)
```
## Struct templates
Structs can be templated like this:
```java
Vector2 :: struct (Scalar: Type) {
  x, y: Scalar
}
```
Now `Vector2` is a struct template, which can be instantiated with parameters specified in parentheses:
```java
a: Vector2(Float)

b := Vector2(Int)(x=1, y=2)
```
## Deducable templates
Consider the following example:
```java
add :: (a, b: Int) => a + b
add :: (a, b: Float) => a + b
```
Bodies of these functions are identical, only argument types are different.
We could reduce this to a single function by using a constant type:
```java
add :: (T: %Type, a, b: T) => a + b

add(Int, 1, 2)
add(Float, 1.2, 2.3)
```
It works, but you always have to explicitly specify the type. It would be nice if compiler
automatically deduced the type for us:
```java
add :: (a, b: $T) => a + b

add(1, 2) // instantiates add with T = Int
add(1.2, 3.4) // instantiates add with T = Float
```
`$T` introduces a deducable type `T`. It will be matched to argument's type.
## Constraints
You can constran a template parameter of a lambda:
```java
length :: (v: Vector2($T)) => sqrt(v.x*v.x + v.y*v.y)
```
`length` can only be called with a `Vector2`, any other type will not match this.

In the above example `($T)` may be omitted:
```java
length :: (v: Vector2) => sqrt(v.x*v.x + v.y*v.y)
```
In that case compiler sees that `Vector2` is a
template struct, and automatically makes this lambda a template. You will not
be able to use `T` directly anymore, but you could access it through `v.Scalar`.
# Overloading
## Basics
This language allows multiple definitions in one scope to have the same name.
This is mostly used with functions, because in most other cases you will not be able to differentiate the definitions.
There are tree ways of overloading functions.
1) Different number of arguments.
```java
foo :: (a: Int)
foo :: (a, b: Int)

foo(1)    // Calls the first one
foo(1, 2) // Calls the second one
```
2) Different argument types.
```java
foo :: (a: Int)
foo :: (a: String)

foo(1)   // Calls the first one
foo("a") // Calls the second one
```
3) Different argument names.
```java
foo :: (a: Int)
foo :: (b: Int)

foo(a=1) // Calls the first one
foo(b=1) // Calls the second one
// foo(1)  // Ambiguous, does not compile.
```
## Resolution
There are rules for overload resolution regarding variadics, templates and `Any` type.
Compiler collects all information about possible overloads and selects one as follows:
1) If argument types match exactly, that overload is picked.
2) Regular arguments are preferred to variadic.
3) Template is preferred to Any
```java
foo :: (a: Int)
foo :: (a: $T)
foo :: (a: Any)

foo(1) // Calls foo(Int)
foo("hello") // Calls foo($T)
x: Any = 1
foo(x) // Calls foo(Any)
```
```java
foo :: (a: Int)
foo :: (a: ..Int)

foo()     // Calls foo(..Int)
foo(1)    // Calls foo(Int)
foo(2, 3) // Calls foo(..Int)
```
# Whitespace and semicolons
Spaces and tabs are not significant:
```java
a : = 12
a:=12
// these are the same thing
```
Before converting the source code into tokens, all new lines (\n, \r or \r\n) are converted to \n.
This affects multiline string literals, lines are always separated by \n.
Maybe there will be a way to provide custom line separators.


Because this language does not require a semicolon after a statement,
new lines in this language are significant in certain places. For example:
```java
a := 1 + 2
// OK
```
```java
a := 1 +
2
// OK, compiler expects an expression to the right of the +.
// It is not on the current line, so it searches in the next one.
```
## TODO: maybe this should work?
```java
a := 1
+ 2
// Error. In this case compiler sees `1` as a single expression,
// because there is a new line right after it. Then it parses `+ 2`,
// which is a valid expression, but not a valid statement,
// so it shows corresponing error.
```
---
As said earlier, semicolons are optional in this language.
They can be used to explicitly terminate a statement
```java
if condition
  a = b + c;
else
  a = b - c;
```
Note that semicolon must be placed on the same line with the statement.
```java
if condition
  a = b + c
  ;
else
  a = b - c;
// Error. The line with the semicolon and the one above are independent
// statements, and we can't use two statements in an `if` without braces.
```
Basically the rule is:
* If a semicolon is present right after a statement (no new line between them), it is included in that statement and the statement is terminated.
* Otherwise it is treated as an independent empty statement.

Note that empty statements and block statements, like `while`, `defer` and others, can not be terminated with a semicolon.
A semicolon after such statement will always be independent
# Compile time queries
```java
#line // line number

#column // column number

#file // file string

#location // string in form "file:line:column"

#function // function type string
// NOTE: this directive includes return type name.
// If the return type is explicitly specified, #function is usable in constant
// context. Otherwise it will result in "undefined". If no return type was
// specified, #function has to be evaluated after typechecking the entire body.
// But constant expressions have to be evaluated as they are encountered, which
// is always before inferring the return type, meaning that you can't use this
// directive in constant context.
paradox :: () {
  #if #function contains "Int" {
    return true // makes #function return "(): Bool"
  } else {
    return 1 // makes #function return "(): Int"
  }
}
////////////////////////////

// TODO: implement
#definition // current definition name
// Example:
Thing :: struct {
  name := #definition // evaluates to "Thing"
  do_stuff :: () {
    return #definition // returns "do_stuff"
  }
}
////////////////////////////

#assert expression // assert at compile time that expression evaluates to `true`

#print expression // prints the value at compile time

#typeof expression // returns the type of expression

#compiles statement // Evaluates to true or false depending if the
                    // following statement compiles or not.
                    // That block will not be evaluated at runtime.

# lambda // evaluate lambda at compile time.
```
# Types
## Builtin types
### Fundamental types
```java
Void
Bool
U8 U16 U32 U64 // Unsigned ints
S8 S16 S32 S64 // Signed ints
F32 F64 // Floats
String
Type // Every type has type `Type`
```
### Structs
```java
String :: struct {
  data: *U8
  count: Int
}
StructMember :: struct {
  name: String
  type: *Typeinfo
  offset: Int
}
EnumMember :: struct {
  value: Int
  name: String
}
TypeInfo :: struct {
  kind: TypeKind
  name: String
  size: Int
  members: []StructMember
  pointee: *TypeInfo
  array_count: Int
  enum_members: []EnumMember
  parameters: []*TypeInfo
}
Any :: struct {
  pointer: *U8
  type: *TypeInfo
}
Range :: struct {
  min, max: Int
}
```
#### TODO: Make Range a template
### Enums
```java
TypeKind :: enum {
	Void,
	Bool,
	U8,
	U16,
	U32,
	U64,
	S8,
	S16,
	S32,
	S64,
	F32,
	F64,
	\struct,
	\enum,
	pointer,
	span,
	array,
}
```
### Aliases
```java
Int SInt // alias to signed integer of target architecture register's size
UInt // alias to unsigned integer of target architecture register's size
Float // alias to F64
```
## Type modifiers
### Pointer
```java
x: *String // x is a pointer to String
```
### Optional
```java
func :: (a: ?String) {
  print(if a then *a else "a is empty")
}
```
### Array
```java
array: [10]Int
array[0] = 12
// pointer to first element is array.data
// number of elements is array.count
```
### Span
Span is just a pointer to an array and the number of elements.
Span does not have to cover an entire array, it can point at a portion of it.
```java
span: []Int
{
  array: [4]Int
  span = array
  // now span covers the entire array.

  span[0] = 14
  print(array[0]) // prints 14

  span.data = &array[1]
  span.count = 2
  // now span includes only elements [1] and [2].

  span[0] = 12
  print(array[1]) // prints 12
}
// array is gone after end of scope, so span is pointing at freed memory,
// which will be used for next variables.
```
There is an implicit conversion from an array to a span.
```java
process :: (span: []Int)
get_array :: (): [4]Int

process(get_array())

// In this example we need to convert an array to a span, but we can't
// get the address of the array because it is returned from a function.
// To allow this conversion, the array is temporarily stored in a reserved stack memory.
// This memory region can be overwritten by subsequent statements.
```
## Internal types
There is a bunch of special types that are available only during compilation.
For example, unsized integer is the initial type of any integer literal.
You can do math with literals and they won't be truncated. They also will preserve the precision if you assign them to names:
```java
big_value :: 1 << 1000 // this won't fit in any sized integer type

println(big_value) // error, does not fit into any type
println(big_value >> 1000)// ok, prints 1
```
Note that checks for too big numbers is not fully implemented yet, so be careful to not overflow your RAM.

# Using
`using` is a keyword that can be used before a definition or an identifier.
It brings all inner names from the definition inside the current scope.
```java
Vector3 :: struct { x,y,z: Float }

vec: Vector3
using vec
// The above two lines can be merged together in a single one:
// using vec: Vector3
x = 1
y = 4
z = 9
println(vec) // prints 1,4,9
```
`using` can be applied to any definition, including globals, locals, parameters, return parameters, struct members.

# Packs
Packs allow you to pass variable amount of arguments of the same type to a function.
```java
vararg :: (pack: ..String) {
  i := 0
  while i != pack.count {
    print(pack[i])
    i += 1
  }
}

vararg()
vararg("hello")
vararg("hello", "world")
```
Basically a pack is just a span. When you pass multiple arguments to a pack, a temporary array
with specified values is created, and a span to it is passed to the function.

Parameter pack does not have to be last in the argument list.
```java
vararg :: (first: Int, middle: ..String, last: Int) {...}

vararg(1, 2)
vararg(1, "hello", 2)
vararg(1, "hello", "world", 2)
```
You can have multiple packs in a function.
```java
vararg :: (a: ..Int, b: ..String) {...}

vararg(1, 2)
vararg(1, "hello")
vararg("hello", "world")
```
An argument will be put into a pack while it implicitly converts to the pack's element type.
This means it is not possible to have subsequent packs of the same type.

If you want to pass a pack to a function that accepts another pack, you have to use `..` syntax:
```java
foo :: (a: ..Int) {...}
bar :: (b: ..Int) => foo(..b)
```
You might think that `..` in `..b` is redundant, because you just passing a span to a function
that accepts the same span, and you will be right in that case.
This will not work with `Any` though. Consider this example:
```java
foo :: (a: ..Any) {...}
bar1 :: (b: ..Any) => foo(..b)
bar2 :: (b: ..Any) => foo(b)
```
`bar1` will call foo with the same received arguments.
`bar2` howewer will call foo with just one argument of type `[]Any`.
# A pointer to a function
```java
ptr: (): Int #type
// the same syntax is used for types. Because of that it's
// ambiguous if it's a type or a lambda without a body.
// So to distinguish between them we have to use #type directive.
```
# Implicit addressing and dereferencing
```java
by_value :: (v: Int) => v * 2
by_pointer :: (v: *Int) => *v * 2

x := 1

by_pointer(x) // address of x is implicitly taken

ptr := &x

by_value(ptr) // ptr is implicitly dereferenced
```

# Properties
A property is a pair of `get`/`set` methods.

To make a getter define a function that starts with `get_` and has one parameter:
```java
get_length :: (v: Vector) => sqrt(v.x*v.x + v.y*v.y)
```
This property is now accessible just like a member of a struct:
```java
v: Vector(1, 2)

println(v.length)

// v.length is transformed into get_length(v)

// Note that v.get_length() and get_length(v) are still valid
```
Setters are defined similarly, by prefixing a function with `set_`, but taking two parameters instead:
```java
set_length :: (v: *Vector, l: Float) => *v = *v * (l / v.length)

v.length = 5 // This becomes set_length(&v, 5)

v.length *= 10 // This becomes set_length(&v, get_length(v) * 10)

```
Note that getter can take the first parameter by pointer, and setter does not have to (not sure why but it's possible).

Also it doesn't have to be a pair. You may have only getter or only setter.

# Operator precedence
All binary operators are left associative and have following precedence:
```java
10:  .
 9:  as
 8:  custom infix
 7:  * / %
 6:  + -
 5:  & | ^ << >>
 4:  == != > < >= <=
 3:  || &&
 2:  ..
 1:  = += *= &= and so on...
```
Example:
```java
// This expression  parses into  this one:
a = b + c * d dot e              (a = (b + (c * (d dot e))))
```
# Temporary space
This language provides a lot of implicit conversions to simplify things,
for example from array to span, or from any type to `Any` type, etc.
These conversions require taking a pointer to something, which is fine if
that something is for example a stack variable, just take its address.
However, what if conversion from a non-addressable value happens?
```java
foo :: (x: Any) { ... }

get_thing :: (): [1024]Int { ... }

foo(get_thing())
```
In this case, you can't just take an address of `get_thing()`.
For that case compiler puts the value returned by `get_thing()` in the temporary region of the stack
and passes the pointer to `Any`. That temporary region is untouched until the scope is closed.
After that that temporary region may be overwritten by subsequent users of the temporary space.

This makes doing something like this possible without much trouble:
```java
x: Any = 2 // that 2 is stored until the scope closes.
```

### TODO: Compare different temporary space sizing strategies.

Right now the size of temporary space is determined per-scope (maximum of all scopes in a function).
This method may use more memory than in per-statement case, but it makes things easier.
Maybe there's a better strategy for determining temporary space size.
Maybe "bind" a temporary allocation to a definition that "invoked" it an subtract the size after the last use of that definition?
I don't know if memory savings are worth the complication.

# Autocast
You can easily cast an expression to a type known by compiler by using `@` operator:
```java
a: Int
b: *Void
// a = b     // error, *Void is not implicitly convertible to *Int.
a = b as Int // can explicitly specify the type.
a = @b       // same as previous, but the type is inferred.
```
If @ operator is used in a context where destination type is not known, it is ignored.
# Type information
You can get information about any type at compile time from `#typeinfo` expression
```java
info := #typeinfo Int
println(info)
```
This information contains most of the things you need to know about the type like size, members and a lot more.

Type information is so simple yet so useful.
```java
Vector2 :: struct {
  x: Float32
  y: Float32
}

println(Vector2(1, 2))
```
Yes, you don't have to print each member manually, everything is printable by default.
# Other things
* All variables are initialized to zero by default.
* Compile time execution (only basic operations are supported for now)
* Global definitions can be ordered in any way
```java
main :: () => print(X)
X :: 1337
```
* Literals are unlimited in size. They will be converted to required type at use site. Compiler will warn you if the value does not fit into destination type.
```java
a :: 999999999999999999999999999 * 123456789
```
* Lossy conversions are always explicit.
# TODO
* [ ] Builtin types
  * [ ] variant
* [ ] SIMD
* [ ] Multiple return parameters
* [ ] Optimization
* [ ] Context
* [ ] Specify build options in the source code
* [ ] Default arguments
* [ ] AST inspection.
* [ ] Caller argument expression string
```java
func :: (a: String) {
  print(#exprof a)
}

func(get_the_string(12, 34))

// this should print "get_the_string(12, 34)"
```