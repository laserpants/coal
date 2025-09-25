# Coal

This repository is the home of the Coal programming language and compiler. The project is under mega-construction. 👷

## About

Coal is a declarative, purely functional programming language with

- simple and intuitive syntax, 
- algebraic data types/pattern matching,
- extensible records, 
- structural recursion/corecursion, 
- traits (type classes), and 
- effect handlers (work in progress)

&hellip; among other features. Coal's type system supports type inference and parametric polymorphism, similar to Haskell, ML, and other languages based on the [System-F](https://en.wikipedia.org/wiki/System_F) lambda calculus. The Coal compiler is written in Haskell and targets [LLVM](https://llvm.org/) for code generation.

### Rethinking recursion

As a [total](https://en.wikipedia.org/wiki/Total_functional_programming) language, Coal takes a different approach to recursion, following the motto that "[recursion is the GOTO of functional programming](https://www.semanticscholar.org/paper/Functional-Programming-with-Bananas%2C-Lenses%2C-and-Meijer-Fokkinga/5db3c6793c07285bf0f5e95fe5a25f53e7488051)." To guarantee that programs are provably terminating, recursion is only available in a restricted form, known as *structural recursion*. In this regime, each recursive call operates on a strictly smaller part of some finite data structure, progressing toward a base case. 

```
  fun sum(numbers : List<int32>) : int32 =
    fold(numbers) {
      | [] => 0 
      | x :: @tot => x + tot
    }
```

A distinction is made between ordinary, finite data, which is produced and consumed in this way, and potentially infinite data &mdash; the kind that may result from processes that run indefinitely. The latter is known as *codata*. The codata equivalent of lists, for example, are streams.

```
  cotype Stream<a> = { Head : a, Tail : Stream<a> }

  unfold enum_from(n : int32) : Stream<int32> {
    , Head = n
    , @Tail = n + 1
  }

  let nats = enum_from(0)
```

These examples show two modes of recursive control flow. If you are familiar with [recursion schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html) in a language like Haskell, recursion in Coal is based on the same principles. 

- **First example:** The special `@`-pattern variable means that `tot` recieves the result from calling the fold again using the sub-list matched by the pattern. 
- **Second example:** The `@` in the field name causes the expression on the right (`n + 1`) to become the next seed value, which is fed back into `enum_from` to generate the rest of the stream.

In the recursion scheme framework, `fold` and `unfold` are called *catamorphisms* and *anamorphisms*. Scroll down to **Recursion, corecursion, and codata** for a more detailed explanation of how these constructs work in Coal.

### Programs = Expressions + Effects

Coal is a highly [expression-oriented](https://en.wikipedia.org/wiki/Expression-oriented_programming_language) language: a program is, at its core, just an expression that evaluates to a value. In this programming model, all data is immutable and there are no observable side-effects. These properties make programs more predictable, easier to reason about, highly testable, and allows for code to be verified using formal mathematical techniques. On the other hand, practical applications need to have the ability to interact with the outside world. Side-effects are what make them useful. A system for managing effects, such as I/O and exceptions, is still lacking in Coal. This is an essential step to promote the language into one that can be used to write actual programs. See **How to contribute** if you're keen to work on this.

## Project status and roadmap

#### Current milestone: 1

The following is a list of features that are either missing or incomplete, and :

| Feature                          | Milestone              | Criteria         |                                                                        
| -------------------------------- | ---------------------- | ---------------- |                                                                        
| Module imports/exports           | 1                      |                  |
| Topological sort                 | 1                      |                  |

#### Roadmap

| Feature                          | Milestone              | Criteria         |                                                                        
| -------------------------------- | ---------------------- | ---------------- |                                                                        
| CLI                              | 2                      | It should be possible to compile programs from the terminal using a command like `coal Main.coal -o example` |                                                                        
| Pattern match totality checking  | 2                      | Partial match statements are reported as errors at compile-time.                                             |                                                                        
| Trait inheritance                | 3                      | Traits like `Show<List<a>> with Show<a>` should work.                                                        |                                                                        
| Qualified (namespace) imports    | 3                      | For example, `import namespace List` works.                                                                  |
| Package system                   | 4                      |                  |                                                                        
| Effects                          | 5                      |                  |                                                                        

## Installation and setup

TODO

### Prerequisites

TODO

### Compiler

TODO 

```
                     +---------------------------------------+                
                     |                                       |
  +--------------+   |   +------------+       +----------+   |   +--------------+
  |              |  >>>  |   Kernel   |  >>>  |   LLVM   |  >>>  |  Executable  |
  +--------------+   |   +------------+       +----------+   |   +--------------+
                     |                                       |
                     +---------------------------------------+
```

## How to contribute

TODO

## Language overview

### Modules and imports

Projects in Coal are organized as collections of *modules*. Modules provide a way to group related functionality into distinct [namespaces](https://en.wikipedia.org/wiki/Namespace). A module can contain functions, type definitions, traits, and other language constructs, typically focused on a specific purpose within a library or application.

```
module <path>(<export_list>) {
  <definition>
  <definition>
  ...
}
```

Every module is uniquely identified by its *path*. A module's path mirrors the directory structure of the source file in which it is defined. Path segments begin with an uppercase letter and are separated by a dot (`.`). File names have a `.coal` suffix. A module `Utils.Math.Trigonometry`, for instance, is defined in a file named `Trigonometry.coal`, located under `Utils/Math/` relative to your project's root directory.

```
src
└── Utils
    └── Math
        └── Trigonometry.coal
```

In a module declaration, the path identifier is followed by an optional list of exported names enclosed in parentheses. Only exported names are visible outside the module (or *public* in OOP terminology).

```
module Utils.Math.Trigonometry(sin, cos, tan) {
  // ...
```

If this list is left out, everything in the module is exported.

#### Imports

An `import` statement is used to bring in functions and other definitions from other modules. As in most other languages, these must appear at the beginning of a module, preceding any other code.

```
import List(concat, head, tail)
```

A *namespace* import allows you to access all functions, types, and other definitions from a module using their qualified names. A qualified name is formed by prefixing the name with the path of the module:

```
// Import the List module under its namespace
import namespace List

  // And use it like this:
  let zs = List.concat(xs, ys)
```

### Top-level definitions

Definitions that occupy the outermost scope of a module are functions, top-level let-expressions, data and codata type definitions, traits, trait instances, folds, and unfolds.

#### Functions

A function is defined using the `fun` keyword, followed by the function's name and a list of comma-separated arguments enclosed in parentheses. The function body is simply an expression, which follows the arguments and is preceded by an equals sign:

```
  fun <name>(<arg_1>, <arg_2>, ..., <arg_n>) =
    <expr>
```

In the above, `<arg_1>, <arg_2>, ..., <arg_n>` are *patterns*, allowing functions to directly deconstruct their arguments. In addition to basic variables, records, tuples, and other data constructors, patterns can also include wildcards, literals, and nested structures. See **Pattern matching** for an overview of available patterns.

```
  fun bork({ n : int32 }, (fst, snd), _) =
    ...
```

A type annotion can be given to indicate a function's return type; as in the following example:

```
  fun is_even(n : int32) : bool =
    n % 2 == 0
```

#### Other expressions

Expressions that are not functions can also be defined in this scope, using the `let` keyword:

```
  let <name> = <expr>
```

A module-level let-binding looks like an ordinary let-expression (explained below), except that there is no expression body:

```
module Utils {

  let days = 
    [ "Monday"
    , "Tuesday"
    , "Wednesday"
    , "Thursday"
    , "Friday"
    , "Saturday"
    , "Sunday" 
    ]

}
```

Since a `let` can hold any other expression, top-level functions may also be defined in the following way:

```
let add = fn(x, y) => x + y     // This is the same as fun(x, y) = x + y
```

#### Main

TODO

```
module Main {

  fun main() =
    ...
    
}
```

### Expression syntax

Expressions are the core building blocks of programs. They include variables, literals, let-bindings, operators, and control structures like `if-then-else`. An expression can often be composed of other, smaller expressions. For example, a binary operator consists of two sub-expressions: its left-hand side and right-hand side operands:

```
  (+)     
  / \     x and y are sub-expressions of the expression x + y
 x   y
```

#### Variables

A *variable* in Coal is simply a name bound to a value. Unlike in imperative languages, it is not very helpful to think of a variable as a “box” that represents some data store in memory. In functional programming, expressions behave more like mathematical expressions: once a variable is defined, its value never changes.

##### Naming rules

Variable names are subject to the following rules:

* A name can consist of letters (`A-Z`, `a-z`), digits (`0-9`), and the underscore character (`_`).
* The first character of a variable name must be a lowercase letter or an underscore.
* Variable names are case-sensitive, meaning that `my_VAR` and `my_var` refer to different variables.
* Variable names cannot contain spaces.
* Special characters other than underscores (e.g., `!`, `#`, `%`, `@`) are not permitted in variable names.
* Reserved language keywords (see below) cannot be used as variable names.

##### Reserved keywords

```
alias, as, bignum, bool, char, cotype, double, else, false, float, fn, fold, fun, if, import, in, instance, int32, int64, let, match, module, nat, or, string, then, trait, true, type, unfold, unit, when, where, with
```

##### Shadowing 

> This feature is not yet implemented.

*Shadowing* is to declare a variable in an inner scope with the same name as an existing variable. This is often a source of subtle bugs &mdash; it is therefore not allowed. An expression such as the following should result in a compilation error:

```
fun go(x) =
  let x = 3 in x + 3
```

#### Literal expressions

TODO

#### Built-in language primitives

Coal provides the following built-in basic language types:

| Type               | Description                             | Example values            |                       
| ------------------ | --------------------------------------- | ------------------------- |                       
| `bool`             | Booleans                                | `true`, `false`         |                       
| `char`             | A single Unicode character              | `'a'`, `'b'`, `'🤖'`, ... |                        
| `float`            | Single precision floating point numbers | `3.1519f`                 |                        
| `double`           | Double precision floating point numbers | `3.141592653589793`       |                        
| `int32`            | 32-bit integers                         | `0`, `1`, `2`, `3`, ...   |                        
| `int64`            | 64-bit integers                         | `0`, `1`, `2`, `3`, ...   |                        
| `bignum`           | Arbitrary precision integers            | `0`, `1`, `2`, `3`, ...   |                        
| `string`           | UTF-8 text                              |  `"Hello, ✨ world!"`     |                        
| `unit`             | Singleton type                          | `()`                      |                        
| `void`             | The uninhabited type                    |                           |                        
| `nat`              | Natural numbers (Peano arithmetic)      | `Zero`, `Succ(Zero)`, ... |                        

##### Integral types

Integer literals introduced in code without an explicit type annotation, such as

```
let answer = 42
```

&hellip; are polymorphic. Their inferred type is `n with Numeric(n)`, which isn't an ordinary type. It rather means that `n` can be *any* type, as long as it is a member of the `Numeric` trait (see **Traits**). 
This includes the built-in `int32`, `int64`, `bignum`, and `nat` types. All `Numeric` types support the basic arithmetic operations of addition, subtraction, and multiplication.

```
fun sum_of(x, y, z) = 
  x + y + z 

let n : int32 = sum(1, 2, 3)
let d : double = sum(0.5, 1.0, 1.5)
```

<!--
```
  // 

  type Complex = Complex(double, double)

  instance Numeric(Complex) {
    // ...
  }
```
-->

#### Function application

Unlike Haskell, ML, and OCaml, function applications are written using parentheses and commas between arguments &mdash; a syntax more akin to languages like C, Java, and Python. 
So, for example, `concat("one", "two")` means the function `concat` applied to the arguments `"one"` and `"two"`.

By default, functions in Coal are *curried*. There is a difference between a function that takes multiple arguments, and one that takes a single tuple as its argument. Consider the following two type signatures:

```
f : a -> b -> c
g : (a, b) -> c
```

The first of these is in curried form, which is usually more convenient to work with. Curried functions can be partially applied. This is useful, for example, when working with higher-order functions. Suppose we define an addition function:

```
fun add(x, y) = x + y
```

Using partial application, we can create a new function `increment` by supplying just one argument:

```
let increment = add(1)
```

Partially applied functions can also be passed directly to a higher-order function like `map`:

```
map(add(1), [1, 2, 3, 4])   // which yields the same result as map(increment, [1, 2, 3, 4])
```

#### If-then-else

If-expressions are similar to those in most programming languages, in particular those in the functional family. Both the `then` and `else` branches need to be present (and have the same type):

```
  if (<e_1 : bool>) then <e_2 : t> else <e_3 : t>
```

```
  if (temperature > 20) then wear("shorts") else go_home()
```

#### Let-bindings

A let-binding introduces a new scope by matching a pattern against the result of an expression. The variables bound by the pattern become available within the expression following the `in` keyword:

```
let <pattern> = <e_1> in <e_2>
```

The pattern used on the left-hand side must be such that it is guaranteed to match the result of the expression `<e_1>`. For example:

```
-- Destructuring with a tuple
let (x, y) = (1, 2) in x + y

-- Matching nested records
let { baz = { f = a | _ } } = faz(4)
```

Variables form the simplest form of pattern; namely one that matches any value and binds it to a name:

```
let name = "Zlatan" 
```

>  #### A note about let-generalization
>
> In some ways, a let-binding is interchangeable with a lambda function. For example, writing `let x = 1 in increment(x)` yields the same result as `(fn(x) => increment(x))(1)`.
> But besides being more readable, the let-binding also has another purpose. In [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) languages, it is let-bindings that introduce polymorphism. Consider the following expression, which doesn't type check:
> 
> ```
>   (fn(f) => (f(3 : int32), f("three")))(fn(x) => x)
> ```
> 
> In this example, the type of `f` is monomorphic. The type inference algorithm will try to determine its type but fail to unify `int32 -> int32` with `string -> string`.
> If we instead bind the anonymous function to a new identifier, then its type is *generalized* and obtains the quantified type `∀a : a -> a` (known as a *type scheme*).
> We can now apply this function to both elements of the tuple, even though they have different types:
> 
> ```
>   let id = fn(x) => x 
>     in 
>       (id(3 : int32), id("three"))
> ```

###### Name binding semantics

A subtle but important detail that makes let-bindings in Coal different from those in most other languages is that the identifier introduced by a `let` is **not in scope within the definition itself**. In other words, `let x = e1 in e2` makes `x` available in `e2`, but not in `e1`. In OCaml (and F#) this is also the case for the standard `let` keyword. However, in these languages, a special `let rec` syntax makes it possible to evade this restriction. Coal doesn't have an equivalent to `let rec`.
This prevents non-well-founded expressions, such as `let f = f in f`, but more generally makes explicit recursion impossible (i.e., for any function to reference itself).
The restriction also applies to top-level definitions. As far as the compiler is concerned, this function:

```
fun fib(n) = if (n == 0 || n == 1) then n else fib(n - 1) + fib(n - 2)
```

&hellip; (defined at the top level) translates into:

```
let fib = fn(n) => if (n == 0 || n == 1) then n else fib(n - 1) + fib(n - 2)
                                                     ^^^
Error: Name "fib" not in scope
```

In fact, one can think of a module as one big let-binding, only laid out in a more readable way:

```
  let
    some_function = fn(...) => ...
      in
        let
          some_other_function = fn(...) => ...
            in
              let 
                main = fn() => 
                  ...
```

This is why functions such as the fibonacci function above are rejected by the compiler. 

#### Lambda expressions

An anonymous (lambda) function is declared with the `fn` keyword and the “fat” arrow (`=>`) symbol:

```
  fn(<arg_1>, <arg_2>, ..., <arg_n>) => <expr>
```

Function expressions are first-class objects; they can be passed as arguments to other functions, assigned and stored inside data structures, etc.

```
  fun app_fst(xs, x : int32) =
     match(xs) {
       | f :: _ => f(x)
       | [] => 0
     }

  fun main() =
    let fns = 
      [ fn(x) => x + 1
      , fn(x) => x + 2
      , fn(x) => x + 3
      ]
    in
      trace_int32(app_fst(fns, 3))
```

Just like with let-bindings, the arguments in a lambda-function are patterns:

```
  fn((lhs, rhs)) => lhs
```

#### Comments

There are two types of comments:

- Single-line comments begin with a double forward slash (`//`) and extend to the end of the line. Any text following `//` is considered a comment.

```
  foo(1)  // Leave any comments about this comment in the comment field below.
```

- Multi-line comments (also called *block comments*) start with `/*` and end with `*/`. All text between these delimiters is treated as a comment.

```
  /* This is a long comment. It can extend over multiple 
     lines. It may or may not contain ASCII art depicting,
     for example, a giraffe. 
  */
  fun sqrt(d : double) =
    ...
```

### Types

##### Unit

The `unit` type has only a single value, written as an empty pair of parentheses: `()`. At a first glance, this type appears to be of no purpose, but it actually has a number of applications. 
In many instances it is useful to be able to express that a function argument isn't used to compute the return value. In C, for example, we can define the following function:

```
int five() {
  /* ... */
  
  return 5;
}
```

This is where the `unit` type comes in handy:

```
fun five(() : unit) : int32 = 5
```

Removing the type annotation, the above becomes `fun five(()) = 5`, which is perfectly valid. But since an expression like `five()` doesn't have any other meaningful interpretation, the compiler accepts this as a shorthand for the slightly awkward double-parentheses form.

```
fun five() = 5   // i.e., fun five(() : unit) = 5
```

Similarly, when calling a function that only takes a unit value as argument, the extra parentheses can be omitted:

```
let 
  x = 
    five()   // we could have written five(()) here
  in
    x + 5
```

Keep in mind that this only works with `unit`. For non-empty tuples, you still need the extra parentheses:

```
fun fst4((fst, _, _, _)) = fst
```

#### Algebraic data types

User-defined data types in Coal are of the product-sum variety. These types are introduced with the `type` keyword. 

- A *product* type combines multiple fields into one single value: All of the components appear together in the constructed data (e.g. an RGB color triplet that contains individual red, green, and blue values).

   ```
   type Color = Rgb(int8, int8, int8)
   ```

- A *sum* type represents a choice between alternatives: A value belongs to exactly one of the specified variants (e.g. a shape that can be either a `Circle` or a `Rectangle`).

   ```
   type Shape = Circle | Rectangle
   ```

More interesting types can be formed from combinations of product and sum constructors. The following example is a type that defines a binary tree, parameterized by the type (`a`) of its nodes:

```
type Tree<a> 
  = Leaf
  | Node(a, Tree<a>, Tree<a>)
```

Here is how a basic tree is described in terms of this type:

```
//          (4)
//          / \
//       ---------
//       /       \
//     (2)       (6)
//    -----     -----
//    /   \     /   \ 
//  (1)   (3) (5)   (7)  

let tree_of_gondor = 
  Node 
    ( 4
    , Node
        ( 2
        , Node(1, Leaf, Leaf)
        , Node(3, Leaf, Leaf)
        )
    , Node
        ( 6
        , Node(5, Leaf, Leaf)
        , Node(7, Leaf, Leaf)
        )
    )
```

Algebraic data types work very well to describe language grammars.

```
  type JsonValue
    = JsonNull
    | JsonBool(bool)
    | JsonNumber(double)
    | JsonString(string)
    | JsonArray(List<JsonValue>)
    | JsonObject(List<(string, JsonValue)>)
```

#### Natural numbers

Recursion in Coal relies on pattern matching to take layered data apart in a stepwise manner,

stepwise peel of layers of data constructors ..?

always working hand-in-hand with a recursive data structure like lists, trees, or other algebraic data types. 

If we want to use an integer counter, similar to how for-loops behaves in imperative programming languages.

Ordinary (machine type) integers are insufficient

do not meet this requirement. 


Instead, we need to define a recursive number type. This is typically done according to the standard axiomatization of the natural numbers:

> Every natural number is either zero or the successor of another natural number.

This is known as the *Peano construction* of the natural numbers, named after the Italian mathematician [Giuseppe Peano](https://en.wikipedia.org/wiki/Giuseppe_Peano).
The definition of the built-in `nat` type is simply this idea expressed in code:

```
type nat
 = Zero
 | Succ(nat)
```

The number five, for example, would then be written:

```
Succ(Succ(Succ(Succ(Succ(Zero)))))
```

Writing numbers in this notation quickly becomes tedious. Fortunately, it is not necessary.

Internally, the compiler stores values of type `nat` as normal integers. 

```
pack_nat : int32 -> nat
unpack_nat : nat -> int32
```

Converting back and forth between these are constant time (**O**(1)) operations.

#### Lists

A list is an ordered collection where all elements are of the same type. Lists are a foundational data structure in functional programming, commonly used to store and manipulate collections of data, and as a building block for implementing other higher-level abstractions.

In Coal, list literals are denoted by a sequence of comma-separated expressions, enclosed in square brackets:

```
[<expr_1 : t>, <expr_2 : t>, ..., <expr_n : t>] : List<t>
```

Example:

```
[1, 1, 2, 5, 14, 42, 132, 429] : List<int32>
```

The `List` type is defined inductively, and implemented as a one-way *linked list* of nodes. This means that a list of type `List<a>` is either (1) the empty list; or (2) a value of type `a` coupled with another `List<a>` list. These last two are usually referred to as the *head* and *tail* of the list. 

```
type List<a>
  = []
  | a :: List<a>
```

#### Option

```
type Option<a>
  = Some(a)
  | None
```

#### Tuples

Just like lists, tuples are ordered sequences of values. Unlike lists, however, a tuple's length is fixed (i.e. determined at compile-time), and its elements can have different types. In code, a tuple is written as a comma-separated sequence of expressions enclosed in parentheses:

```
(<expr_1 : t_1>, <expr_2 : t_2>, ..., <expr_n : t_n>) : (t_1, t_2, ..., t_n)
```

##### Examples:

```
(10, "covfefe", false)  // The type of this tuple is: (int32, string, bool)
```

Tuples of length two and three are often called *pairs* and *triples*, respectively. There is no singleton tuple type &mdash; a single value in parentheses is just the value itself:

```
(42)  // Not a tuple -- just the integer 42
```

The empty tuple *does* exist, and has special meaning. It is written `()` and is known as the unit value. The type of `()` is `unit`. (See **Built-in types** for more details.)

```
()            : unit                           // unit value
(1, 2)        : (int32, int32)                 // 2-tuple
(1, 2, 3)     : (int32, int32, int32)          // 3-tuple
(1, 2, 3, 4)  : (int32, int32, int32, int32)   // 4-tuple
// ...
```

##### Tuples and currying

To specify a tuple as the only argument to a function, you need to use an extra pair of parentheses:

```
fun add((a, b)) = a + b

let five = add((1, 4))
```

The `curry` and `uncurry` combinators convert an uncurried function into a curried one, and vice versa.

```
curry : ((a, b) -> c) -> a -> b -> c
uncurry : (a -> b -> c) -> (a, b) -> c
```

Here is how `curry` is used with the uncurried version of `add`, to change it into curried form.

```
let five = curry(add, 1, 4)         // or (curry(add))(1, 4)
```

#### Records

Records are unordered collections of name-value pairs in which the values can be of any type, including other records. The record type is suitable for representing structured data with multiple properties, and nested objects: A record is written as a sequence of comma-separated *fields* enclosed in curly braces. A field consists of a name, referred to as the *label*, paired with a value. These two are separated by an equals sign (`=`). 

```
{ 
  name = "Eros Ramazzotti", 
  shoe_size = 43.0, 
  privileges = ["read", "edit", "karaoke"]
}
```

A valid type for the above record is:

```
{ name : string, shoe_size : float, privileges : List<string> }
```

The notation to describe the type of a record looks similar to the record itself, except that the fields consist of the label and the field's type. So, instead of the equals sign, a colon (:) appears after the label.

Since the order of fields is irrelevant, the following two records are considered identical:

```
{ x = 1, y = 2 }
{ y = 2, x = 1 }
```

The naming rules for labels are the same as those for variables; all characters must be either alphanumeric or an underscore (_), and the first character cannot be a number. 

##### Field access

The contents of a field of a record can be obtained using the field access operator. The syntax is just a dot (`.`) followed by the field's label: 

```
let language = { name = "Java", paradigm = "OOP" }
  in language.name
```

##### Extending records

Records in Noll are said to be **extensible**, meaning that it is possible to add fields to a record at run-time.

Example:

```
fun tagged(rec, t : string) = { tag = t | rec }  
```

The function in this example accepts two arguments; an existing record rec, and a string t. It returns a copy of rec augmented with a new field labelled tag. This new field assumes the value of the argument t. The pipe symbol (|) is an infix operator which takes the record on the right-hand side of the expression and extends it with the list of fields on the left. So, in this example, if we have a record r = { day = "monday", humidity = 73.5 }, and apply tagged to (r, "wet"), then we get back a new record:

```
{ day = "monday", humidity = 73.5, tag = "wet" }
```

What makes this especially useful is that the type of the original record doesn't matter. Its labels and field types do not have to be known at compile time.

Also notice that the expression on the left-hand side of the pipe is a list of fields, so any number of fields can be added at once: 

```
{ a = 1, b = 2 | { c = 3 } } 
  == { a = 1 | { b = 2 | { c = 3 } } } 
  == { a = 1 | { b = 2, c = 3 } }
  => { a = 1, b = 2, c = 3 }  
```

Open and closed records

Here is the function signature for `tagged` again, this time with added type annotations:

```
tagged(rec : { | r }, t : string) : { tag : string | r } = 
  { tag = t | rec }
```

These types look a bit different from earlier examples. Here, the pipe (|) symbol appears in the type of the record. It serves a similar purpose to that of its expression-level counterpart. The type variable r represents, what is known as, a row, which is essentially a type-level list of fields. We refer to a record type of this variety as open. This is different from closed records, where all the fields are explicitly specified in the type. 
The following example further illustrates the difference between open and closed records. The record in these examples represents a GPS coordinate pair with two fields; lat and lng, for latitude and longitude respectively.
fn(p : { lat : float, lng : float }) => p.lat
This function requires the argument's fields to exactly match the fields in the type signature. The type in this example is closed. 
fn(p : { lat : float, lng : float | q }) => p.lat
This function, on the other hand, is polymorphic in the row variable q. It can be called with any record, as long as its type includes the labels lat and lng, and both of these fields have type float. For instance, 
{ lat =-3.067425, lng = 37.355625, alt = 5895 } , 
{ location = "Great Pyramid", time = "2024-09-15T10:57:19Z", lat = 29.9792, lng = 31.1342 }, and 
{ lat = 0.0, lng = 1.0 },
are all accepted. This type is open. The general format of an open record type is 
{ <label_1> : <t_1>, <label_2> : <t_2>, ..., <label_n> : <t_n> | <r> },
for some n ≥ 0.  Recall the earlier example involving the function tagged and the type of the argument rec in that function:
rec : { | r }
In this type we want the type variable r to capture all available fields, and so n becomes 0. That is how we end up with the funny-looking type { | r }.

### Pattern matching

The `match` expression in Coal lets you to deconstruct data based on its shape, reversing what the data constructors of algebraic data types do.

TODO

#### Patterns

Variable, tuple, record, etc..

### Traits

A *trait* describes a collection of functions that must be defined for a given type.

```
trait <name>(<type_parameter>) {
  <definition_1>: <type_1> 
  <definition_2>: <type_2> 
  ...
  <definition_n>: <type_n> 
}
```

By defining a set of behaviors as a trait, you can reuse the same functionality across all types that support it. This reduces duplication and encourages reusable code. Traits are conceptually similar to type classes in Haskell and a common analogy is to think of them as interfaces in object-oriented programming.

The following example defines a trait with a single function, `compare`. This function takes two inputs *a* and *b* of the same type and returns a value to indicate if *a* is less than *b* (`Lt`), greater than (`Gt`), or if the two values are equal (`Eq`). In other words, this trait captures the notion of a [total order](https://en.wikipedia.org/wiki/Total_order) on the type `t`.

```
trait Ordered<t> {
  fun compare : t -> t -> Order   // where type Order = Lt | Gt | Eq
}
```

Making a type support a trait involves defining an *instance* of that trait. An instance provides concrete implementations of all functions declared in the trait, specialized for the chosen type. 
For example, by instantiating the `Ordered` trait for `bool`, we define an ordering on the booleans:

```
instance Ordered<bool> {
  fun compare(a, b) =
    match((a, b)) {
      (false, true) => Lt
      (true, false) => Gt
      (_, _) => Eq
    }
}
```

Code that uses `compare` now works uniformly for all types that have an `Ordered` instance:

```
fun is_less_than(x : t, y : t) : bool with Ordered<t> =
  compare(x, y) == Lt
```

Here, the type parameter `t` in the type of `is_less_than` is *universally quantified*. The `with` keyword introduces one or more constraints on type variables appearing in a type. In this case it demands that an instance of `Ordered` exists for the type `t`.
We can read the type of `is_less_than` as: `t -> t -> bool with Ordered<t>`.

#### Higher-kinded traits

TODO

<!--

```
trait Functor<f : * -> *> {
  map : (a -> b) -> f<a> -> f<b>;
}
```

Recall that the `Option` type is defined as:

```
type Option<a>
  = Some<a>
  | None
```

```
// Make Option an instance of the Functor trait
instance Functor<Option> {
  fun map(f, opt) =
    match(opt) {
      | Some(a) => Some(f(a))
      | None => None
    }
}
```

baz

```
map(fn(x) => x * 100, Some(1))    // ==> Some(100)
map(fn(x) => x * 100, [1, 2, 3])  // ==> [100, 200, 300]
```
-->

#### Trait inheritance

TODO

### Recursion, corecursion, and codata

In most languages, a typical implementation of the factorial function looks something like the following:

```
fun factorial(n : int32) =
  if (n == 0)
    then 1
    else n * factorial(n - 1)
```

If we pass this function to the Coal compiler, it is rejected with the following error:

```
  |       else n * factorial(n - 1);
  |                ^^^^^^^^^

Name not in scope: factorial
```

Referencing a function from within itself in this way is not possible in Coal. Instead, recursion needs to be expressed in terms of a pattern know as a *fold*. 
A fold takes some collection of data and combines it into a single result. A common instance is where an array of numbers is reduced into a single value, for example by continually adding each number to the parital sum.

```
let sum = reduce(fn(n, a) => f + a, [1, 2, 3])
```

Note that `fold` is a language keyword in Coal, not an ordinary function. 
Folds are similar to `match` expressions, but with some extra powers.

We are going to use the `nat` data type to define the factorial function:

```
  fun factorial(n : nat) =
    fold(n) {
      | Zero =>
          1
      | Succ(@p) as m =>
          m * p
    }
```

The magic happens _ the `@`-pattern used in the second clause. 

Note that `p` is not an ordinary variable. .. and evaluates fold recursively with the value 

The result is the same as ...

```
      | Succ(r) as m => m * fold(r)
```

There are specific rules as to how this pattern can be used.
Most importantly, it can only appear inside a constructor. 
For recursion to be well-defined, progress must be guaranteed in each iterative step. 
The constructor rule is how this is enforced by the language. 
The data inside of the constructor is structurally smaller 

The following is therefore not possible:

```
    fold(n) {
      | @p => p
    }
```

#### Top-level folds and mutual recursion

The type of folds we have seen so far are ...

```
module Json {

  import String(intercalate)

  type JsonValue
    = JsonNull
    | JsonBool(bool)
    | JsonNumber(double)
    | JsonString(string)
    | JsonArray(List<JsonValue>)
    | JsonObject(List<(string, JsonValue)>)

  fold encode_json_value : JsonValue -> string {
    | JsonNull => "null"
    | JsonBool(false) => "false"
    | JsonBool(true) => "true"
    | JsonNumber(d) => double_to_string(d)
    | JsonString(str) => "\"" +++ str +++ "\""
    | JsonArray(encode_json_array(@values)) => "[" +++ intercalate(",", values) +++ "]"
    | JsonObject(encode_json_object(@key_value_pairs)) => "{" +++ intercalate(",", key_value_pairs) +++ "}"
  }

  fold encode_json_array : List<JsonValue> -> List<string> {
    | [] => []
    | encode_json_value(@value) :: encode_json_array(@values) => value :: values
  }

  fold encode_json_object : List<(string, JsonValue)> -> List<string> {
    | [] => []
    | (key, encode_json_value(@value)) :: encode_json_object(@pairs) => 
        let label = "\"" +++ key +++ "\"" 
        in 
        (label +++ ":" +++ value) :: pairs
  } 

  fun encode_json(value : JsonValue) = encode_json_value(value)

}
```

#### Duality

Data and codata can be seen as two sides of the same coin. This *duality* goes deeper than mere superficial resemblance.
The idea originates in category theory, where folds and unfolds have very precise meanings.
An algebraic data type can be seen as the [initial algebra](https://en.wikipedia.org/wiki/Initial_algebra) of a functor: it provides the smallest, well-founded solution that can be consumed by a fold (a catamorphism).
In the other direction, a codata type corresponds to the [final coalgebra](https://en.wikipedia.org/wiki/Initial_algebra#Final_coalgebra) of a functor: it is the largest (potentially infinite) solution that can be observed or generated by an unfold (an anamorphism).
In this description, algebras and coalgebras are mirror images: by simply reversing the direction of the arrows in their diagrams, an algebra turns into a coalgebra and vice versa.

|                    | Access pattern        | Structure             | Evaluation strategy  | Invariant               |
| ------------------ | ----------------------| --------------------- | -------------------- | ----------------------- |
| **Data**           | Recursion (fold)      | Always finite         | Eager (strict)       | Progress                |
| **Codata**         | Corecursion (unfold)  | Potentially infinite  | Lazy (non-strict)    | Productivity            |

## License 

This project is licensed under the terms of the MIT license. See the `LICENSE` file in this repository for details.

<!--

---
---
---
---




A more idiomatic version of the factorial function ...

```
  fun factorial(n : int32) =
    product(enum_to(n))  // product of numbers 1, 2, ..., n

```

#### Pattern matching

Just like with other data types, tuples can be deconstructed by means of pattern matching.

```
fun get_name((name, _, _) : (string, t1, t2)) : string = 
  name 
```

asfd

```
fun fst3((fst, _, _) : (a, b, c)) : a = fst
fun snd3((_, snd, _) : (a, b, c)) : b = snd
fun thd3((_, _, thd) : (a, b, c)) : c = thd 
```

### Expression syntax

One useful perspective is to think of traits as algebraic structures in mathematics.

```
trait Group<g> {
  combine : g -> g -> g
  inverse : g -> g
  identity : g
}
```

```
type Additive = AddInt32(int32)

instance Group<Additive> {
  combine = fn(AddInt32(x), AddInt32(y)) => AddInt32(x + y)
  inverse = fn(AddInt32(x)) => AddInt32(-x)
  identity = AddInt32(0)
}
```

## Effects as a side business

Similar to how the user interface describes
is a 
we can think of an effect boundary as similar to a user interface — but instead 
of mediating between a human and a program, it mediates between pure logic and 
the external world. Pure code produces structured descriptions of effects, and 
effect handlers at the boundary interpret those descriptions to perform real-world actions.



<!--


##### The unit type

The unit value is written as an empty pair of parentheses. It is the 
one-and-only value of type `unit`. 

```
() : unit
```

One useful perspective is to think of traits as algebraic structures in mathematics.

```
trait Group(g) {
  combine : g -> g -> g
  inverse : g -> g
  identity : g
}
```

```
type Additive = AddInt32 int32

instance Group(Additive) {
  combine = fn(AddInt32(x), AddInt32(y)) => AddInt32(x + y)
  inverse = fn(AddInt32(x)) => AddInt32(-x)
  identity = AddInt32(0)
}
```

```
main : IO()
main = 
  readFile("file") 
    >>= putStrLn
    >> return()
```

Semantic effects

```
main =
```

#### Recursion

In a total language, all programs are provably terminating, and (at least in
theory) have no runtime bugs. On the other hand, a limitation that applies
to all languages with this property is that they fail to be Turing complete.
In practical terms, this means that, in Coal, recursion is only allowed in a
restricted form, known as structural recursion.

A trivial proof for this is that a Turing machine has the ability to go into an infinite loop, and this would mean a contradiction when computations are invariably required to terminate. 


the limitation imposed by the compiler is that 

```
let <name> = <expr> in <body>
```

A function defined at the top level is (technically) a let-binding, which means that the function itself is not in scope inside the function body:

```
let fact = 
  fn(n) => 
    if (n == 0)
      then 1 
      else n * fact(n - 1) // <-- This doesn't work
  in fact(5)
```

In OCaml, for example, this same rule applies with regards to the standard `let` 
construct, but this can easily be overridden using the `let rec` keyword. As an
aside, Coal actually has a recursive let binding too, but it is only available
to the compiler.

A function defined at the top level is (technically) a let-binding, which means that the function itself is not in scope inside the function body:

```
fact(n) =
  if (n == 0) then 1
              else n * fact(n - 1) // <-- No cigar
```

based on a  .. known as recursion schemes 
The fold keyword and the special @-syntax used in the following example implements a pattern known as a catamorphism in this paradigm. 

```
factorial(n : nat) =
  fold(n) {
    | Zero => 
        1
    | Succ(@m) =>
        n * m          // m == fold(m)
  }
```

The key here is the built-in `nat` data type.

The built-in nat data type defines the natural numbers inductively, as follows: 

```
// Peano construction of the counting numbers
type nat
  = Zero
  | Succ(nat)
```

The utility of this type is that we can pattern match to 

The compiler translates the _ into something that looks like the following:

```
factorial(n : nat) =
  letrec
    fold(n) =
      match(n) {
        | Zero => 1
        | Succ(m) => n * fold(m)
      }
```

This is pseudo-code since letrec doesn't really exist as a language keyword. 

#### Corecursion and codata

Despite of this limitation, infinite data structures and non-terminating behavior still exist in Coal. They are represented using codata and corecursion. 

```
codata Stream(s) =
  { Head : s
  , Tail : Stream(s) 
  }
```

asdf

```
nats : Stream(int32)
nats = seq(0) 
seq(n).Head = n
seq : int32 -> Stream(int32)
seq = Stream(next, n) {
  Head = n, 
  Tail = next(n + 1)
}
head : Stream(a) -> a
head(Stream{ Head = head, Tail = _ }) = head
// ...
seq : int32 -> Stream(int32)
seq(n) = Stream{
  Head = n, 
  Tail(@next) = next(n + 1) 
}
zeros = Stream(next) {
  Head = 0, 
  Tail = next
}
type StreamImpl(s) =
  fn(f) => { Head = ?, Tail = f }
```

-->
