# Coal

This repository is the home of the **Coal programming language** and compiler. 

> 🚧 The project is under mega-construction. 🚧

1. [About](#about)
1. [Project status and roadmap](#project-status-and-roadmap)
1. [Language overview](#language-overview)
1. [Installation and setup](#installation-and-setup)
1. [How to contribute](#how-to-contribute)
1. [License](#license)

## About

Coal is a declarative, statically typed, purely functional programming language with

- simple and intuitive syntax
- algebraic data types and pattern matching
- extensible records
- structural recursion 
- codata and corecursion
- traits (type classes)

among other features. Coal’s type system, like Haskell’s and ML’s, supports type inference and parametric polymorphism, drawing on the [System-F](https://en.wikipedia.org/wiki/System_F) lambda calculus. The Coal compiler is implemented in Haskell and targets [LLVM](https://llvm.org/) for code generation.

### Rethinking recursion

As a [total](https://en.wikipedia.org/wiki/Total_functional_programming) language, Coal takes a different approach to recursion, following the motto that "[recursion is the `goto` of functional programming](https://www.semanticscholar.org/paper/Functional-Programming-with-Bananas%2C-Lenses%2C-and-Meijer-Fokkinga/5db3c6793c07285bf0f5e95fe5a25f53e7488051)." To guarantee that programs are provably terminating, recursion is only available in a restricted form, known as *structural recursion*. Under this regime, each recursive call operates on a strictly smaller part of some finite data structure, progressing toward a base case. 

```
  fun sum(numbers : List<int32>) : int32 =
    fold(numbers) {
      | [] => 0 
      | x :: @tot => x + tot
    }
```

The special `@`-pattern variable used here makes `tot` recieve the result from calling the fold again using the sub-list matched by the pattern. 

A distinction is made between ordinary, finite data, which is produced and consumed in this way, and potentially infinite data &mdash; the kind that may result from processes that run indefinitely. The latter is known as *codata*. The codata equivalent of lists, for example, are *streams*.

```
  cotype Stream<a> = { Head : a, Tail : Stream<a> }

  unfold enum_from(n : int32) : Stream<int32> {
    , Head = n
    , @Tail = n + 1
  }

  let nats = enum_from(0)
```

In this example, the `@` in the field name causes the expression on the right (`n + 1`) to become the next seed value, which is fed back into `enum_from` to generate the rest of the stream.

These code samples illustrate two distinct modes of recursive control flow. If you are familiar with [recursion schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html) in a language like Haskell, recursion in Coal is based on the same principles. In that framework, `fold` and `unfold` are called *catamorphisms* and *anamorphisms*, respectively. Jump to **[Recursion, corecursion, and codata](#recursion-corecursion-and-codata)** for a more detailed discussion.

### Programs = Expressions + Effects

Coal is a highly [expression-oriented](https://en.wikipedia.org/wiki/Expression-oriented_programming_language) language: a program is, at its core, just an expression that evaluates to a value. In this programming model, all data is immutable and there are no observable side-effects. These properties make programs more predictable, easier to reason about, highly testable, and allows for code to be verified using formal mathematical techniques. On the other hand, practical applications need to have the ability to interact with the outside world. Side-effects are what make them useful. A [system for managing effects](https://en.wikipedia.org/wiki/Effect_system), such as I/O and exceptions, is still lacking in Coal. This is an essential step to promote the language into one that can be used to write actual programs. See **[How to contribute](#how-to-contribute)** if you’re keen to work on this.

## Project status and roadmap

### Roadmap

#### Next milestone: 1

![](https://geps.dev/progress/40)

| Milestone  | Feature/Fix                                           | Milestone  | Feature/Fix                       | Criteria                                                                                              |                                                                                                                                                                                                                                      
| ---------- | ----------------------------------------------------- | -----------| --------------------------------  | ----------------------------------------------------------------------------------------------------- |                                                                                                                                                                                                                                      
| 1          | Module imports/exports                                | 2          | Compiler monolith                 |                                                                                                       |                                                                                                                                                              
| 1          | Operators                                             | 2          | CLI                               | It is possible to compile programs from the terminal using a command like `coal Main.coal -o example` |                                                                         
| 1          | Translation pipeline                                  | ☑️         | <strike>Pattern match totality checking</strike> | <strike>Partial match statements are reported as errors at compile-time.</strike>      |                                                                         
| 1          | Default instance for Numeric                          | 3          | Trait inheritance                 | Traits like `Show<List<a>> with Show<a>` should work.                                                 |                                                                         
| 1          | Bug: double precision                                 | 3          | Qualified (namespace) imports     | `import namespace X` works.                                                                           |                                                                         
| 1          | Standard library                                      | 4          | Package system                    |                                                                                                       |                                                                        
| ☑️         | <strike>Basic documentation</strike>                  | 5          | Effects                           |                                                                                                       |                                                                        
| ☑️         | <strike>Semicolon-separators in let-bindings</strike> | 5          | User-defined operators            |                                                                                                       |                                                                        
| 1          | Error messages                                        |            |                                   |                                                                                                       |                                                                        

## Language overview

### Table of contents

  1. [Modules](#modules)
     - [Imports](#imports)
     - [Exports](#exports)
  1. [Top-level definitions](#top-level-definitions)
     - [Functions](#functions)
     - [Let-expressions](#let-expressions)
     - [Data types](#data-types)
  1. [Built-in language primitives](#built-in-language-primitives)
  1. [Expression syntax](#expression-syntax)
     - [Variables](#variables)
       - [Naming rules](#naming-rules)
       - [Reserved keywords](#reserved-keywords)
       - [Shadowing considered harmful](#shadowing-considered-harmful)
     - [Literal expressions](#literal-expressions)
       - [Integral types](#integral-types)
     - [Function application](#function-application)
     - [If-then-else](#if-then-else)
     - [Let-bindings](#let-bindings)
       - [Name binding semantics](#name-binding-semantics)
     - [Lambda expressions](#lambda-expressions)
     - [Operators](#operators)
       - [Arithmetic and comparison](#arithmetic-and-comparison)
       - [Logical](#logical)
       - [Data](#data)
       - [Function composition and pipelining](#function-composition-and-pipelining)
       - [List operations](#list-operations)
       - [String manipulation](#string-manipulation)
     - [Comments](#comments)
  1. [Types](#types)
     - [Natural numbers](#natural-numbers)
     - [Unit](#unit)
     - [List](#list)
       - [Common list operations](#common-list-operations)
       - [Useful higher-order list functions](#useful-higher-order-list-functions)
       - [List predicates](#list-predicates)
     - [Option](#option)
     - [Tuples](#tuples)
     - [Records](#records)
       - [Field access](#field-access)
       - [Extending records](#extending-records)
       - [Open and closed records](#open-and-closed-records)
       - [Pattern matching over records](#pattern-matching-over-records)
       - [Deconstructing records](#deconstructing-records)
  1. [Pattern matching](#pattern-matching)
     - [Totality requirement](#totality-requirement)
     - [Wildcard patterns](#wildcard-patterns)
     - [Lambda match](#lambda-match)
     - [Supported patterns](#supported-patterns)
  1. [Traits](#traits)
     - [Higher-kinded traits](#higher-kinded-traits)
     - [Trait inheritance](#trait-inheritance)
  1. [Recursion, corecursion, and codata](#recursion-corecursion-and-codata)
     - [Fold syntax](#fold-syntax)
       - [Well-foundedness](#well-foundedness)
       - [Beyond factorial](#beyond-factorial)
     - [Top-level folds and mutual recursion](#top-level-folds-and-mutual-recursion)
     - [Codata and unfold](#codata-and-unfold)
       - [Data on demand](#data-on-demand)
       - [A basic counter](#a-basic-counter)
     - [Duality](#duality)

### Modules 

Projects in Coal are organized as collections of *modules*. Modules provide a way to group related functionality into distinct [namespaces](https://en.wikipedia.org/wiki/Namespace). A module can contain functions, type definitions, traits, and other language constructs, typically focused on a specific purpose within a library or application.

```
module <path>(<export_list>) {
  <definition>
  <definition>
  ...
}
```

Every module is uniquely identified by its *path*. 

- A module’s path mirrors the directory structure of the source file in which it is defined. 
- Path segments begin with an uppercase letter and are separated by a dot (`.`). 
- Files have a `.coal` extension. 

A module `Utils.Math.Trigonometry`, for instance, is defined in a file named `Trigonometry.coal`, located under `Utils/Math/` relative to your project’s root directory:

```
src
└── Utils
    └── Math
        └── Trigonometry.coal
```

#### Imports

An `import` statement is used to bring in functions and other definitions from other modules. These must appear at the beginning of a module, preceding any other code.

```
import List(concat, head, tail)
```

The special `namespace` keyword allows you to import and access all functions, types, and other definitions from a module via their *qualified* names. A qualified name is formed by prefixing the name with the path of the module:

```
// Import the List module under its namespace
import namespace List

  // And use it like this:
  let zs = List.concat(xs, ys)
```

#### Exports

In a module declaration, the path identifier is followed by an optional list of exported names enclosed in parentheses. Only exported names are visible outside the module (or *public* in OOP terminology).

```
module Utils.Math.Trigonometry(sin, cos, tan) {
  // ...
```

If this list is left out, everything in the module is exported.

### Top-level definitions

Definitions that occupy the outermost scope of a module are functions, top-level let-expressions, data and codata type definitions, traits, trait instances, folds, and unfolds.

#### Functions

A function is defined with the `fun` keyword, followed by the function’s name and a list of comma-separated arguments enclosed in parentheses. The function body is simply an expression, which comes after the arguments and is preceded by an equals sign:

```
  fun <name>(<arg_1>, <arg_2>, ..., <arg_n>) =
    <expr>
```

Function parameters are *patterns*, allowing functions to directly deconstruct their arguments. In addition to basic variables, records, tuples, and other data constructors, patterns can also include wildcards, literals, and nested structures. See **[Pattern matching](#pattern-matching)** for a list of available patterns.

```
  fun bork({ n : int32 }, (fst, snd), _) =
    ...
```

A type annotion can be given to indicate a function’s return type, as in the following example:

```
  fun is_even(n : int32) : bool =
    n % 2 == 0
```

##### Main

Just like in many other programming languages, the `main` function serves as the entry point of a program:

```
module Main {

  fun main() =
    ...
```

#### Let-expressions

The `let` keyword introduces a new name bound to the result of an expression. Inside functions, a `let` is often used to give names to intermediate values:

```
  fun hypotenuse(a, b) =
    let sqr_a = a * a;
        sqr_b = b * b
    in 
      sqrt(sqr_a + sqr_b)
```

Here, `sqr_a` and `sqr_b` are local bindings, only visible in the body that follows the `in`.

At the top level of a module, a `let` works in the same way, except there is no enclosing body — the binding simply introduces a global name that can be referenced elsewhere in the module (or from other modules):

```
  let days = 
    [ "Monday"
    , "Tuesday"
    , "Wednesday"
    , "Thursday"
    , "Friday"
    , "Saturday"
    , "Sunday" 
    ]
```

Type annotations for let-bindings look similar to those for functions:

```
  let days : List<string> =
    [ "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ]
```

Since a `let` can hold any expression, top-level functions can also be defined this way:

```
  let add = fn(x, y) => x + y
  // Equivalent to:
  // fun add(x, y) = x + y
```

#### Data types

User-defined data types in Coal are introduced using the `type` keyword. They are of the product-sum variety.

A *product* type combines multiple fields into one single value: All of the components appear together in the constructed data. An RGB color triplet that contains individual red, green, and blue values can be described with a type:

```
type Color = Rgb(int8, int8, int8)
```

A *sum* type is a choice between alternatives: A value belongs to exactly one of the specified variants. A type that represents a shape that can be either a `Circle` or a `Rectangle` can be defined as:

```
type Shape = Circle | Rectangle
```

More complex types can be built by combining product and sum constructors. The following is a type that defines a binary tree, parameterized by the type (`a`) of its nodes:

```
type Tree<a> 
  = Leaf
  | Node(a, Tree<a>, Tree<a>)
```

This definition says that a `Tree<a>` is either:

- a `Leaf` (the empty tree), or
- a `Node` containing a value of type `a` along with two sub-trees (the left and right branches).

Using this type, we can represent any finite binary tree. For example, here is a tree of integers:

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

The structure of the tree is entirely determined by its constructors (`Leaf` and `Node`), which makes recursion natural. 

Algebraic data types are especially useful for describing language grammars and other hierarchical structures. Consider this JSON representation:

```
  type JsonValue
    = Null
    | Bool(bool)
    | Number(double)
    | String(string)
    | Array(List<JsonValue>)
    | Object(List<(string, JsonValue)>)
```

### Built-in language primitives

Coal provides the following built-in types:

| Type               | Description                             | Example values            |                       
| ------------------ | --------------------------------------- | ------------------------- |                       
| `bool`             | Booleans                                | `true`, `false`           |                       
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

### Expression syntax

Expressions are the core building blocks of programs. They include variables, literals, let-bindings, operators, and control structures like `if-then-else`. An expression can often be composed of other, smaller expressions. For example, a binary operator consists of two sub-expressions: its left-hand side and right-hand side operands:

```
  (+)     
  / \     x and y are sub-expressions of the expression x + y
 x   y
```

#### Variables

A *variable* in Coal is simply a name bound to a value. Unlike in imperative languages, it is not very meaningful to think of a variable as a “box” that represents some data store in memory. In functional programming, expressions behave more like mathematical expressions: once a variable is defined, its value never changes.

##### Naming rules

Variable names are subject to the following rules:

* A name can consist of letters (`A-Z`, `a-z`), digits (`0-9`), and the underscore character (`_`).
* The first character of a variable name must be a lowercase letter or an underscore.
* Variable names are case-sensitive, meaning that `my_VAR` and `my_var` refer to different variables.
* Variable names cannot contain spaces.
* Special characters other than underscores (e.g., `!`, `#`, `%`, `@`) are not permitted in variable names.

##### Reserved keywords

Reserved language keywords cannot be used as variable names. These are:

```
alias, as, bignum, bool, char, cotype, double, else, false, float, fn, fold, fun, if, import, in, instance, int32, int64, let, match, module, nat, or, string, then, trait, true, type, unfold, unit, when, where, with
```

##### Shadowing considered harmful

*Shadowing* occurs when a variable declared in an inner scope has the same name as a variable from an outer scope. 

```
fun go(x) =
  let x = 3 in x + 3
```

In this example, the inner `let` attempts to declare a new variable that has the same name as the function parameter, namely `x`.

Because shadowing is often a source of subtle bugs, the Coal compiler treats it as an error.

#### Literal expressions

A *literal* is an expression that directly represents a fixed value of one of the [built-in primitive types](#built-in-language-primitives), such as integers, booleans, or strings.

##### Integral types

Integer literals introduced in code without an explicit type annotation, such as

```
let answer = 42
```

are polymorphic. The inferred type of this expression is `n with Numeric(n)`, which isn’t an ordinary type. It means that `n` can be *any* type, as long as it implements the `Numeric` trait (see **[Traits](#traits)**). This includes the built-in `int32`, `int64`, `bignum`, and `nat` types. All `Numeric` types support the basic arithmetic operations of addition, subtraction, and multiplication.

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

Unlike Haskell, ML, and OCaml, Coal uses parentheses and commas to separate arguments in function applications — a syntax more similar to languages like C, Java, or Python. For example:

```
concat("one", "two")
```

This applies the function `concat` to the arguments `"one"` and `"two"`.

By default, functions are *curried*. There is a difference between a function that takes multiple arguments, and one that takes a single tuple as its argument. Consider the following two type signatures:

```
f : a -> b -> c
g : (a, b) -> c
```

The first of these is in curried form, which is usually more convenient to work with. Curried functions can be partially applied. This is useful, for example, when working with higher-order functions. Suppose we define an addition function:

```
fun add(x, y) = x + y
```

Using partial application, we can create a new function `increment` by supplying just one argument to `add`:

```
let increment = add(1)
```

Partially applied functions can also be passed directly to a higher-order function like `map`:

```
map(add(1), [1, 2, 3, 4])   // which yields the same result as map(increment, [1, 2, 3, 4])
```

#### If-then-else

If-expressions in Coal are similar to those found in many programming languages, especially other functional languages. Both the `then` and `else` clauses must be present, and they must produce values of the same type:

```
  if (<e_1 : bool>) then <e_2 : t> else <e_3 : t>
```

For example:

```
  if (temperature > 20) then wear("shorts") else go_home()
```

#### Let-bindings

A let-binding introduces a new scope by matching a pattern against the result of an expression. The variables bound by the pattern become available within the expression following the `in` keyword:

```
let <pattern> = <e_1> in <e_2>
```

Variables form the simplest form of pattern, namely one that matches any value and binds it to a name:

```
let name = "Zlatan" 
```

The pattern used on the left-hand side must be such that it is guaranteed to match the result of the expression `<e_1>`. For example:

```
-- Destructuring with a tuple
let (x, y) = (1, 2) in x + y

-- Matching nested records
let { baz = { f = a | _ } } = faz(4)
```

> #### A note about let-generalization
>
> In some sense, a let-binding is interchangeable with a lambda function. For example, writing `let x = 1 in increment(x)` yields the same result as `(fn(x) => increment(x))(1)`.
> But besides being more readable, the let-binding also serves another purpose; in [Hindley-Milner](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) languages, it is let-bindings that introduce polymorphism. Consider the following expression, which doesn’t type check:
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

##### Name binding semantics

A subtle but important detail that makes let-bindings in Coal different from those in most other languages is that the identifier introduced by a `let` is **not in scope within the definition itself**. In other words, `let x = e1 in e2` makes `x` available in `e2`, but not in `e1`. In the ML-family of languages (e.g. OCaml), this is also the case for the standard `let` keyword. However, in these languages, a special `let rec` syntax makes it possible to evade this restriction. Coal doesn’t have an equivalent to `let rec`.
This prevents non-well-founded expressions, such as `let f = f in f`, but more generally, makes it impossible for any function to refer to itself. 
The restriction also applies to top-level definitions. For example, as far as the compiler is concerned, the function

```
fun fib(n) = if (n == 0 || n == 1) then n else fib(n - 1) + fib(n - 2)
```

translates into:

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

This is why functions such as the fibonacci function above are straight out rejected by the compiler. 

#### Lambda expressions

An anonymous (lambda) function is declared with the `fn` keyword and the “fat” arrow (`=>`) symbol:

```
  fn(<arg_1>, <arg_2>, ..., <arg_n>) => <expr>
```

Function expressions are first-class objects; they can be passed as arguments to other functions, assigned and stored inside data structures, etc.

```
  fun apply_fst(xs, x : int32) =
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
      trace_int32(apply_fst(fns, 3))
```

Just like with let-bindings, the arguments in a lambda-function are patterns:

```
  fn((lhs, rhs)) => lhs
```

#### Operators

##### Arithmetic and comparison

|               | Description            | Type                                 |                                                                        
| ------------- | ---------------------- | ------------------------------------ |                                                                        
| `+`           | Addition               | `∀n : n -> n -> n with Numeric(n)`   |                                                                        
| `-`           | Subtraction            | `∀n : n -> n -> n with Numeric(n)`   |                                                                        
| `*`           | Multiplication         | `∀n : n -> n -> n with Numeric(n)`   |                                           
| `/`           | Division               |                                      |                                                                        
| `^`           | Exponentiation         | `∀n : n -> nat -> n with Numeric(n)` |                                                                        

|               | Description            | Type                                  |                                                                        
| ------------- | ---------------------- | ------------------------------------- |                                                                        
| `==`          | Equality               | `∀n : n -> n -> bool with Equal(n)`   |                                                                        
| `!=`          | Inequality             | `∀n : n -> n -> bool with Equal(n)`   |                                                                        
| `<`           | Less than              | `∀n : n -> n -> bool with Ordered(n)` |                                           
| `>`           | Greater than           | `∀n : n -> n -> bool with Ordered(n)` |                                                                        
| `<=`          | Less than or equal     | `∀n : n -> n -> bool with Ordered(n)` |                                           
| `>=`          | Greater than or equal  | `∀n : n -> n -> bool with Ordered(n)` |                                                                        

|               | Description            | Type                          |                                                                        
| ------------- | ---------------------- | ----------------------------- |                                                                        
| `%`           | Modulus                |                               |                                                                        

##### Logical

|               | Description            | Arity      | Type                   |                                                                         
| ------------- | ---------------------- | ---------- | ---------------------- |                                                                        
| `&&`          | AND                    | 2          | `bool -> bool -> bool` |                                                                        
| `\|\|`        | OR                     | 2          | `bool -> bool -> bool` |                                                                        
| `!`           | NOT                    | 1          | `bool -> bool`         |                                                                        

##### Data

|               | Description            |                                       |                                                          
| ------------- | ---------------------- | ------------------------------------- |                                                                      
| `.`           | Record field access    | See **[Field access](#field-access)** |                                                               

##### Function composition and pipelining

|               | Description                 | Type                             |                                                                        
| ------------- | --------------------------- | -------------------------------- |                                                                        
| `>>`          | Forward composition         | `(a -> b) -> (b -> c) -> a -> c` |                                                                        
| `<<`          | Reverse composition         | `(b -> c) -> (a -> b) -> a -> c` |                                                                        
| `\|.`         | Reverse application         | `a -> (a -> b) -> b`             |                                                                        
| `.\|`         | Forward application         | `(a -> b) -> a -> b`             |                                                                        
| `$.`          | Flipped reverse application | TODO                             |                                                                        
| `.$`          | Flipped forward application | TODO                             |                                                                        

##### List operations

|               | Description            | Type                                 |                                                                         
| ------------- | ---------------------- | ------------------------------------ |                                                                        
| `++`          | List concatenation     | `∀a : List<a> -> List<a> -> List<a>` |                                                                        

##### String manipulation

|               | Description            | Type                            |                                                                         
| ------------- | ---------------------- | ------------------------------- |                                                                        
| `+++`         | String concatenation   | `string -> string -> string`    |                                                                        

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

         (\-/)
        (:O O:)
         \   /o\
          | |\o \  
          (:) \ o\  
               \o \--_ 
               ( o O
               (  O
  */
  fun sqrt(d : double) =
    ...
```

### Types

#### Natural numbers

Recursion in Coal is closely tied to pattern matching: we peel off layers of a recursive data structure step by step, until reaching its base case. This works naturally with lists, trees, and other algebraic data types. Ordinary machine integers (`int32`, `int64`), however, cannot be pattern matched on. Nevertheless, we often want to use numbers in recursive computations &mdash; for example, when repeating an action, or simulating the behavior of loops in imperative languages. To describe numbers in a way compatible with recursion, we take guidance from the standard axiomatization of the natural numbers:

> Every natural number is either zero or the successor of another natural number.

This is known as the *Peano construction* of the natural numbers, named after the Italian mathematician [Giuseppe Peano](https://en.wikipedia.org/wiki/Giuseppe_Peano). In code, the Peano numbers are expressed as the built-in type `nat`:

```
type nat
 = Zero
 | Succ(nat)
```

The number five, for example, can then be written:

```
Succ(Succ(Succ(Succ(Succ(Zero)))))
```

This makes it possible to use numbers directly in patterns, just like with other algebraic data types:

```
  match(n : nat) {
    | Zero    => "yay"
    | Succ(_) => "nay"
  }
```

Writing numbers in this style quickly becomes impractical, however. To make working with naturals convenient (and efficient), the compiler internally represents values of type `nat` as ordinary integers. Converting between the two views is called *packing* and *unpacking*. These are constant time (O(1)) operations:

```
pack   : int32 -> nat
unpack : nat -> int32
```

#### Unit

The `unit` type has only a single value, written as an empty pair of parentheses: `()`. At first glance this type may appear to serve no purpose, but it has several practical uses. For example, it is often useful to indicate that a function does not take any meaningful input. In C, we might write the following function:

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

##### Two pairs of parentheses for the price of one

Removing the type annotation, the above becomes `fun five(()) = 5`, which is perfectly valid. But since an expression like `five()` doesn’t have any other meaningful interpretation, the compiler accepts this as a shorthand for the slightly awkward double-parentheses.

```
fun five() = 5   // i.e., fun five(() : unit) = 5
```

Similarly, when calling a function that only takes a unit value as argument, the extra parentheses can be omitted:

```
let 
  x = 
    five()   // we could have written five(()) here, but less is more
  in
    x + 5
```

Keep in mind that this only works with `unit`. For non-empty tuples, you still need the extra parentheses:

```
fun fst4((fst, _, _, _)) = fst
```

#### List

A *list* is an ordered collection in which all elements share the same type. Lists are one of the most fundamental data structures in functional programming. They are commonly used to store and manipulate collections of data, and serve as a building block for many higher-level abstractions.

In Coal, list literals are written as a sequence of comma-separated expressions enclosed in square brackets:

```
[<expr_1 : t>, <expr_2 : t>, ..., <expr_n : t>] : List<t>
```

For example:

```
[1, 1, 2, 5, 14, 42, 132, 429] : List<int32>
```

Lists are defined inductively and implemented internally as a [singly linked list](https://en.wikipedia.org/wiki/Linked_list). This means that a list of type `List<a>` is either:

1. the empty list `[]`; or
2. a value of type `a` (the *head*) followed by another list of type `List<a>` (the *tail*).

In pseudo-code:

```
type List<a>
  = []
  | a :: List<a>
```

Here `::` denotes the *cons*-operator, which constructs a new list by prepending an element to an existing list.

Lists can be deconstructed using pattern matching. For example, the following function removes the first element from a list if it happens to be a zero:

```
  fun remove_head_if_zero(list) = 
    match(list) {
      | [] => []
      | head :: tail =>
          if (head == 0)
            then tail     // remove the first element, if it is zero
            else list     // otherwise return the original list
    }
```

This style of unpacking data is common with all algebraic data types (see **[Pattern matching](#pattern-matching)**).

You can also match lists using literal patterns. The following example matches a list of exactly three elements and checks if they form a [Pythagorean triple](https://en.wikipedia.org/wiki/Pythagorean_triple):

```
  fun is_pythagorean(numbers) =
    match(numbers) {
      | [a, b, c] =>
          a^2 + b^2 == c^2 || 
          a^2 + c^2 == b^2 || 
          b^2 + c^2 == a^2
      | _ =>
          false
    }
```

##### Common list operations

The function `length` returns the number of elements in a list:

```
length([0, 1, 2, 3, 4])   // returns 5
```

Its type is:

```
length : List<a> -> nat
```

Since lists are laid out in memory as a chain of nodes connected by pointers, the time complexity of many list operations, including `length`, is O(n).

###### Head, tail, and uncons

- `head` returns the first element of a list, wrapped in an `Option` (described below) to account for the empty list.
- `tail` returns all elements except the first, also as an `Option`.
- `uncons` combines the two: it returns both the head and tail as a tuple, or `None` if the list is empty. In a sense, it undoes what the cons (`::`) constructor does.

These functions take constant (O(1)) time.

```
head : List<a> -> Option<a>
tail : List<a> -> Option<List<a>>
uncons : List<a> -> Option<(a, List<a>)>
```

> #### Function pipelining
> 
> The operator `|.` is used in the following examples. It is an infix operator that performs function application, but with the arguments reversed. So, for example, the expression 
> ```
>   xs |.map(f)
> ```
> is really syntactic sugar for `map(f, xs)`. This operator is very convenient when chaining together multiple function calls. Suppose we have the following basic drawing API:
>
> ```
> circle       : Config -> Shape
> fill         : string -> Shape -> Shape
> set_position : float -> float -> Shape -> Shape
> draw_shape   : Shape -> Canvas -> Canvas
> ```
> 
> To describe a sequence of steps that creates a circle, sets properties such as its color and position, and finally places it on the canvas, we would normally write:
>
> ```
> draw_shape(set_position(10, 5, fill("blue", circle({ radius = 5.0 }))), canvas)
> ```
>
> Using the reverse function application operator (and the associated `$.`-operator), we could instead write the above in a more readable *pipeline*-style:
>
> ```
> circle({ radius = 5.0 })
>   |.fill("blue")
>   |.set_position(10.0, 5.0)
>   $.draw_shape(canvas)
> ```

###### Take, drop and slice

Use `take` to get another list with the first *n* elements from a given list:

```
take : nat -> List<a> -> List<a>
```

For example:

```
[1, 2, 3, 4, 5, 6, 7] |.take(3)     // [1, 2, 3]
```

Note that, if the list’s length is less than the requested number of elements, then `take` returns the entire list. So, for example, `take(5, [1, 2, 3])` returns `[1, 2, 3]`. As expected, `take(0)` always returns an empty list.

The function `drop` removes the first *n* elements from a list.

```
drop : nat -> List<a> -> List<a>
```

For example:

```
[1, 2, 3, 4, 5, 6, 7] |.drop(3)     // [4, 5, 6, 7]
```

If you attempt to drop a greater number of elements than what the list contains, `drop` returns an empty list.

Combining `drop` and `take` allows you to obtain a range of elements from within a list:

```
[1, 2, 3, 4, 5, 6, 7] 
  |.drop(2)
  |.take(3)

// == [3, 4, 5]
```

The function `slice` does exactly this, in a way that allows you to specify the range of elements to extract from the input list:

```
slice : nat -> nat -> List<a> -> List<a>
```

```
[1, 2, 3, 4, 5, 6, 7] |.slice(2, 5)
// == [1, 2, 3, 4, 5, 6, 7] |.drop(2) |.take(5 - 2)
// == [3, 4, 5]
```

###### List concatenation

The list concatenation operator (`++`) appends one list to the end of another, resulting in a new list.

```
  let s = ["Khufu", "Hatshepsut", "Akhenaten"] ++ ["Tutankhamun"]
```

**Note:** The time complecity of `++` is linear (O(n)) in the length of the first list.

<!--
###### Sorting

TODO
-->

##### Useful higher-order list functions

These are functions that take some other function as input, and modify the given list in some way based on the behavior of this function.

###### Mapping over a list

The function `map` applies a function to each element of a list.

For example:

```
[0, 1, 2, 3, 4] |.map(fn(x) => 2 ^ x)       // [1, 2, 4, 8, 16]
```

The type of `map` is:

```
map : (a -> b) -> List<a> -> List<b>
```

> #### Mapping and `Functor`s
>
> The actual type of `map` is more general than the specialized `List` version above. In fact, any value of type `f<a>` can be mapped over, as long as `f` implements the `Functor` [trait](#traits):
>
> ```
> map : (a -> b) -> f<a> -> f<b> with Functor<f>
> ```
>
> We can think of this more abstractly as **"transforming values inside a fixed context"**. In mathematical terms, this corresponds to a structure-preserving map, also known as a *homomorphism*. 
> Homomorphisms are the topic of study in category theory. It is also in category theory that we find the origin of functors. A functor, in this context, is a mapping between categories &mdash; one that sends objects and morphisms from one category to another (subject to certain laws). 
> 
> There are two ways to interpret `map`; we can think of it as a function that applies the function argument to a value of type `a`, in the `f`-context, which could be a list of values, or an optional. The other is that `map` takes some function `a -> b` and *lifts* it into one that acts on `f`-values &mdash; that is, one of type `f<a> -> f<b>`. This second interpretation is more in line with the definition of a functor in category theory. In programming languages, objects correspond to types, and morphisms are simply functions. We then have:
>
> ```
> a           ==>  f<a>                         // The functor transforms objects
> z : a -> b  ==>  map(z) : f<a> -> f<b>        // and functions
> ```
> 
> Functors are expected to obey the following two laws:
>
> ##### 1. Identity law
>
> ```
> map(id) == id
> ```
>
> This says that mapping the identity function over a functor doesn’t change the structure or its contents. 
>
> ##### 2. Composition law
>
> ```
> map(f << g) == map(f) << map(g)     // The operator `<<` denotes function composition, so `f << g = f(g(x)))`.
> ```
>
> This law ensures that mapping the composition of two functions is the same as first mapping one function and then the other. In other words, functors preserve function composition. 
>
> Together, these laws guarantee that mapping behaves consistently: the shape of the container is unchanged, and each element inside the context is transformed individually, and in the same way as if the function were applied directly to that element. These laws aren’t enforced by the compiler, but following them is always a good idea. 
> 

<!--
> ##### List
>
> ```
> instance Functor<List> {
>   fun map(f, xs) =
>     fold(xs) {
>       | [] => []                // (1)
>       | x :: @xs = f(x) :: xs   // (2)
>     }
> }
> ```
>
> ###### Identity law:
> 
> For the `List` instance to satisfy this law, we must have that:
> 
> ```
> map(id, xs) === id(xs)
> ```
>
> For an empty list, ..
>
> ```
> map(id, []) == []      // Follows from (1)
>             == id([])  // By the definition of id
> ```
>
> ###### The other law (?):
>
> The claim here is that, for any list `xs`:
>
> ```
> map(f << g, xs) == (map(f) << map(g))(xs)
> ```
>
> Inductive hypothesis:
> 
> Base case:
> Inductive step:
> 
-->

###### Filtering a list

Filtering is a technique for removing all elements of a list, except those that meet a given condition.

For example:

```
[0, 1, 2, 3, 4] |.filter(fn(x) => x > 2)    // [3, 4] 
```

The type of `filter` is:

```
filter : (a -> bool) -> List<a> -> List<a>
```

That is, `filter` takes a [predicate](#list-predicates) and a list as input, and returns a new list with only the elements that return `true` for the predicate.

###### Reducing a list

The higher-order function `reduce` takes a list and combines its elements into a single result. A common example is reducing a list of numbers to a single value by repeatedly applying an operation, such as summing each element with a running total:

```
let sum = reduce(fn(n, a) => n + a, 0, [1, 2, 3])
```

> The operation described here is also commonly referred to as a *fold*. That name is not used, however, since it is a reserved language keyword in Coal. Along with the special `@`-pattern syntax, it provides the foundation for implementing recursive functions, including `reduce`. This is explored in detail in **[Recursion, corecursion, and codata](#recursion-corecursion-and-codata)**.

The type of `reduce` is:

```
reduce : (e -> a -> a) -> a -> List<e> -> a
```

- The first argument is a function that combines an element of type `e` with an *accumulator* of type `a` to produce a new accumulator.
- The second argument is the initial value of the accumulator.
- The third argument is the list to reduce.

###### Examples of using `reduce`

Concatenating strings:

```
let words = ["Hello", " ", "world", "!"]
let sentence = reduce(fn(w, a) => w +++ a, "", words)
// sentence = "Hello world!"
```

Finding the maximum element:

```
let max_val = reduce(fn(n, a) => if n > a then n else a, -Infinity, [3, 7, 2, 9])
// max_val = 9
```

Counting elements satisfying a condition:

```
let numbers = [1, 2, 3, 4, 5]
let number_of_evens = reduce(fn(n, a) => if n % 2 == 0 then a + 1 else a, 0, numbers)
// number_of_evens = 2
```

<!--
###### Left vs. right folds

TODO
-->

##### List predicates

A *predicate* is a function that tests for some condition with respect to its argument and returns `true` or `false`. By convention, functions that serve this purpose are often prefixed with `is_`. The below predicates are available in the standard `List` package:

```
is_empty     : List<a> -> bool
is_nonempty  : List<a> -> bool
is_singleton : List<a> -> bool
```

###### `is_empty`

A common operation on lists is to check if a list is empty or not. This is what the function `is_empty` does. 

```
is_empty([])                                   // true
is_empty(["wheat", "oats", "rye", "barley"])   // false
```


###### `is_nonempty`

This is the opposite of `is_empty`. That is: 

```
is_nonempty(xs) <==> ! is_empty(xs)
```

###### `is_singleton`

This function returns `true` when the input list has precisely **one** element. 

```
is_singleton(["oats"])                             // true  
is_singleton([])                                   // false
is_singleton(["wheat", "oats", "rye", "barley"])   // false
```

#### Option

The `Option` type is a built-in algebraic data type that represents *optional* values &mdash; values that may or may not be present. This type is called `Maybe` in Haskell and is similar to `Option` in languages like Rust or Scala. 

```
type Option<a>
  = Some(a)
  | None
```

Since `match` statements in Coal need to be exhaustive, `Option` is useful to express the fact that a value cannot be produced in certain cases. For example, let’s say that we are trying to define a function `head`, returning the first element of a list:

```
  fun head(list : List<a>) : a =
    match(list) {
      | head :: _ => head
      | [] => // 💥 What should I return here?
    }
```

The type of this function would be:

```
head : List<a> -> a
```

We can read this type as: Given any type `a` and a list of elements of this type, return an `a` value. That is to say; we know nothing about `a`, except that the list’s elements has this type. 
Therefore, if the input list is empty, then we have nothing to look at. `Option` solves this problem. The `head` function provided by the standard `List` package is defined in the following way: 

```
  fun head(list : List<a>) : Option<a> =
    match(list) {
      | head :: _ => Some(head)
      | [] => None
    }
```

#### Tuples

Just like lists, tuples are ordered sequences of values. Unlike lists, however, a tuple’s length is fixed (i.e. determined at compile-time), and its elements can have different types. In code, a tuple is written as a comma-separated sequence of expressions enclosed in parentheses:

```
(<expr_1 : t_1>, <expr_2 : t_2>, ..., <expr_n : t_n>) : (t_1, t_2, ..., t_n)
```

For example:

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

As with other data types, tuples can be deconstructed by means of pattern matching:

```
  fun fst3((fst, _, _) : (a, b, c)) : a = fst
  fun snd3((_, snd, _) : (a, b, c)) : b = snd
  fun thd3((_, _, thd) : (a, b, c)) : c = thd 
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

Records are unordered collections of name–value pairs, where the values can be of any type, including other records. In Coal, records are first-class values. They are suitable for representing structured data with multiple properties, or nested objects. A record expression is written as a sequence of comma-separated *fields* enclosed in curly braces. Each field consists of a name, called the *label*, paired with a value. The two are separated by an equals sign (`=`):

```
{ 
  name = "Robert Sixkiller", 
  shoe_size = 43.0f, 
  privileges = ["read", "edit", "karaoke"]
}
```

The corresponding type for the above record is:

```
{ name : string, shoe_size : float, privileges : List<string> }
```

The type of a record resembles the expression itself, except that each field is written as a label followed by its type. Instead of an equals sign, a colon (`:`) separates the label and the type.

Since the order of fields is irrelevant, the following two records are considered identical:

```
{ x = 1, y = 2 }
{ y = 2, x = 1 }
```

The naming rules for labels are the same as for variables: labels must consist of alphanumeric characters or underscores (`_`), and the first character cannot be a digit.

##### Field access

The contents of a record field can be obtained using the field-access operator, which is simply a dot (`.`) followed by the field’s label:

```
let language = { name = "Java", paradigm = "OOP" }
  in language.name
```

##### Extending records

Records in Coal are *extensible*, meaning that new fields can be added to a record at run time.

For example:

```
fun tagged(rec, t : string) = { tag = t | rec }  
```

This function accepts two arguments: an existing record `rec` and a string `t`. It returns a copy of `rec` augmented with a new field `tag` which assumes the value of `t`. The pipe symbol (`|`) is an infix operator that takes the record on the right-hand side and extends it with the fields on the left.

For example, if we define a record `r = { day = "monday", humidity = 73.5 }` and apply `tagged(r, "wet")`, we obtain a new record:

```
{ day = "monday", humidity = 73.5, tag = "wet" }
```

What makes this especially useful is that the type of the original record does not matter; its labels and field types need not be known at compile time.

The left-hand side of the pipe is itself a list of fields, so any number of fields can be added at once:

```
{ a = 1, b = 2 | { c = 3 } } 
  == { a = 1 | { b = 2 | { c = 3 } } } 
  == { a = 1 | { b = 2, c = 3 } }
  => { a = 1, b = 2, c = 3 }  
```

##### Open and closed records

Here is the function signature for `tagged` again, this time with added type annotations:

```
tagged(rec : { | r }, t : string) : { tag : string | r } = 
  { tag = t | rec }
```

These types look a bit different from earlier examples. Here, the pipe (`|`) also appears at the type level. It serves a similar purpose: combining fields with an existing record type. The type variable `r` represents a *row*, which can be thought of as a type-level list of fields. A record type of this form is called *open*. By contrast, a *closed* record type explicitly lists all its fields. The following example illustrates the difference. Suppose we want to represent GPS coordinates with two fields, `lat` and `lng`:

```
fn(p : { lat : float, lng : float }) => p.lat
```

In this example, the function requires its argument `p` (a record) to have exactly two fields: `lat` and `lng`, both of type `float`. This type is closed.

```
fn(p : { lat : float, lng : float | q }) => p.lat
```

This function, on the other hand, is polymorphic in the row variable `q`. It accepts any record that includes `lat` and `lng` (both floats), regardless of any additional fields.
For instance, all of the following are valid:

- `{ lat =-3.067425, lng = 37.355625, alt = 5895 }` , 
- `{ location = "Great Pyramid", time = "2024-09-15T10:57:19Z", lat = 29.9792, lng = 31.1342 }`, and 
- `{ lat = 0.0, lng = 1.0 }`,

This type is open. The general format of an open record type is 

```
{ <label_1> : <t_1>, <label_2> : <t_2>, ..., <label_n> : <t_n> | <r> },
```

for some *n* ≥ 0. Recall the earlier `tagged` example and the type of the argument `rec` in that function:

```
rec : { | r }
```

In this type, the variable `r` captures all fields of the input record, so *n* is zero. This explains the somewhat unusual-looking type `{ | r }`.

##### Pattern matching over records

As with other data types, it is possible to pattern match on records. In this context, the right-hand side of a field acts as the binding pattern used to match the sub-expression. The simplest case is to bind a field directly to a variable:

```
  fun full_name({ first_name = fn, last_name = ln }) = fn +++ " " +++ ln 
```

##### Deconstructing records

The pipe (`|`) operator allows you to deconstruct records by matching against a subset of their fields:

```
  fun get_name({ name = n | _ }) = n
```

The right-hand side pattern must be either a variable or a wildcard (`_`). If you use a variable here, it will capture the remainder of the record (all fields not explicitly matched). A common use case is to remove one or more fields from a record. For example:

```
  fun drop_name({ name = _ | fields } : { name : string | q }) : { | q } = fields
```

Here, the name field is removed and a record with all remaining fields are returned.

If you only need to retrieve a single field, the dot syntax (`record.field`) is simpler and more concise. [Pattern matching](#pattern-matching) becomes necessary when you want to extract multiple fields at once, remove fields, or work with the remainder of a record.

##### Updating a field

By combining field extension with pattern matching, you can replace an existing field in a record. For instance, here is a function that updates the `tag` field:

```
  fun set_tag({ tag = _ | fields }, new_tag : string) =
    { tag = new_tag | fields }    
```

This proceeds in two steps: first remove the old field using pattern matching, then reinsert it with the new value. With type annotations:

```
  fun set_tag(
    { tag = _ | fields } : { tag : string | r }, 
    new_tag : string
  ) : { tag : string | r } = 
    { tag = new_tag | fields }
```

This function requires not only that the `tag` field is present, but also that it has the expected type. For example, `{ tag = false }` would be rejected, since `tag` is required to have type `string`.

### Pattern matching

The `match` expression in Coal is used to deconstruct data based on its shape, effectively reversing what the data constructors of algebraic data types do. It allows you to branch on the structure of a value and directly bind its components to variables. For example:

```
  type Shape = Rectangle(float, float) | Circle(float)
  
  fun area(shape) : float =
    match(shape) {
      | Rectangle(w, h) => w * h
      | Circle(r)       => pi * r^2
    }
```

A case in a match expression is called a *clause* and consists of a pattern on the left and an expression on the right. Pattern matching proceeds by checking each clause in order until it finds one whose pattern matches the value. The corresponding right-hand side expression is then evaluated, with any variables in the pattern bound to the matched sub-components.

```
  match(list : List<int32>) {
    | [a]       => a
    | [a, _]    => a
    | [a, _, _] => a
    | _         => 0
  }
```

Variables introduced by a pattern are only in scope in the corresponding right-hand side expression:

```
  match(opt) {
    | Some(x) => x + 1  // x is bound here
    | None    => 0      // x is not in scope here
  }
```

Patterns can take several forms, including data constructors, literals, tuples, records, variables, wildcards, or combinations of these: 

```
  match(shape) {
    | Rectangle(0.0, _) => "flat rectangle"
    | Rectangle(w, h)   => "rectangle with width " +++ show(w)
  }
```

See [below](#supported-patterns) for a complete list of available patterns. 

#### Totality requirement

For a function to be *total*, it must be defined for all inputs of its corresponding type. A consequence of this in the context of `match` expressions is that all possible cases for a type need to be covered by the patterns. In other words, the patterns must be *exhaustive*. If a case is missing, the compiler will reject the program. 

For example, the following function

```
  fun head(input) =
    match(input) {
      | x :: xs => x
    }
```

will produce an error:

```
4:5:
  |
  |     match(input) {
  |     ^^^^^^^^^^^^^^
  |       | x :: xs => x
  | ^^^^^^^^^^^^^^^^^^^^
  |     }

Non-exhaustive patterns
```

#### Wildcard patterns

A *wildcard* pattern is a pattern that matches any value without binding it to a name and is written as an underscore (`_`). These are often useful to guarantee exhaustiveness in `match` expressions. For instance, we can use literal patterns along with a wildcard when matching on integers:

```
  fun describe_int(n : int32) : string =
    match(n) {
      | 0 => "zero"
      | 1 => "one"
      | _ => "something else"
    }
```

#### Lambda match 

A lambda match is a special syntax that lets you get rid of the variable in a `match` expression. For example, this expression:

```
  match {
    | [] => true
    | _ => false 
  }
```

is a shorthand version of this:

```
  fn(val) =>
    match(val) {
      | [] => true
      | _ => false
    }
```

#### Supported patterns

| Type               | Example              | Description                                                                                      |                                                   
| ------------------ | -------------------- | ------------------------------------------------------------------------------------------------ |                                                   
| Constructor        | `Color(r, g, b)`     | Matches a value built with a specific data constructor, binding sub-components to variables.     |                                                 
| Variable           | `x`                  | Matches any value and binds it to the variable.                                                  |                                                 
| Wildcard           | `_`                  | Ignores the matched value (see above).                                                           |
| Literal            | `"Hello"`, `0`, `()` | Matches values that are exactly equal to the given literal.                                      |                                                 
| List constructor   | `x :: xs`            | Matches a list by separating it into head and tail.                                              |                                                 
| List literal       | `[f, s, t]`          | Matches a list of fixed length with elements matching the given sub-patterns.                    |                                                 
| Tuple              | `(lhs, rhs)`         | Matches a tuple by decomposing it into its components.                                           |                                                 
| Record             | `{ name = n \| _ }`  | Matches a record by specifying patterns for one or more fields. See **[Pattern matching over records](#pattern-matching-over-records)** for details. |                                                 
| As                 | `(lhs, _) as pair`   | Matches the inner pattern, while also binding the entire value to a variable.                    |                                                 
| @                  | `Succ(@n)`           | Fold recursion. See **[Recursion, corecursion, and codata](#recursion-corecursion-and-codata)**. |                                                 
| Or                 | `1 or 2`             | Matches if the value satisfies at least one of the given alternative patterns.                   |      

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

The following example defines a trait with a single function, `compare`. This function takes two inputs *a* and *b* of the same type and returns a value to indicate if *a* is less than *b* (`Lt`), greater than (`Gt`), or if the two values are equal (`Eq`). In other words, this trait captures the notion of a [total order](https://en.wikipedia.org/wiki/Total_order) on the type `t` (similar to Haskell’s `Ord` type class).

```
trait Ordered<t> {
  fun compare : t -> t -> Order   // where type Order = Lt | Gt | Eq
}
```

Making a type support a trait comes down to defining an *instance* of the trait. An instance provides concrete implementations of all functions declared in the trait, specialized for the chosen type. For example, by instantiating the `Ordered` trait for `bool`, we define an ordering on the booleans:

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

// is_less_than(3, 5)
// is_less_than(false, true)
```

Type parameters, like `t` in the type of `is_less_than` are [universally quantified](https://en.wikipedia.org/wiki/Universal_quantification). The `with` keyword introduces one or more constraints on type variables appearing in a type. In this case it demands that an instance of `Ordered` exists for the type substituted for `t`.
We write the full type of `is_less_than` as: `t -> t -> bool with Ordered<t>`.

#### Higher-kinded traits

So far, all the traits we’ve looked at have been of the form `T<t>`, where `t` is a placeholder for an ordinary type. Unlike these types, a *type constructor* is a type-level function which takes one or more types as arguments and returns a type. That is, a type constructor on its own isn’t really a type, until it is provided with all necessary type arguments. For example, in the type `Option<int>`, `Option` is a type constructor with [kind](https://en.wikipedia.org/wiki/Kind_(type_theory))

```
* -> *
```

where `*` denotes a *proper* type (i.e., a fully applied type with no parameters). We can read `Option : * -> *` as:

> Option is a type constructor that takes a type as input and produces a type.

This generalizes to constructors of higher kinds. For example, `Result<e, a>` has kind `* -> * -> *`.

Traits can be parameterized by type constructors. Instead of `T<t : *>`, we then get a trait of the form `T<f : * -> ... -> *>`. A common example is the `Functor` trait, which abstracts the idea of [mapping a function](#mapping-over-a-list) over some container-like structure:

```
trait Functor<f : * -> *> {
  map : (a -> b) -> f<a> -> f<b>;
}
```

The `Option` type forms a `Functor` in the following way:

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

This instance ensures that `map` can be used on `Option` values, just like on lists:

```
let times100 = fn(x) => x * 100

map(times100, Some(1))    // ==> Some(100)
map(times100, [1, 2, 3])  // ==> [100, 200, 300]
```

#### Trait inheritance

A trait can declare that it depends on another trait by *inheriting* from it. The inheriting trait is then able to access to the methods of the parent trait, and build its own functionality on top of them. For example, the following instance defines how to display an `Option<a>` value, provided that there is already a way to display values of type `a`:

```
  trait Show<Option<a>> with Show<a> {
    fun show(opt) =
      match(opt) {
        | Some(v) => "Some(" +++ show(v) +++ ")"
        | None => "None"
      }
  } 
```

Here, the `Show<Option<a>>` instance inherits from `Show<a>`. The compiler will only accept this instance assuming a `Show` implementation for `a` is available. Inside the trait body, we can call `show(v)` on the inner value `v : a`. The parent trait `Show<a>` guarantees that `show` is defined for this type. In other words, the ability to show an `Option<a>` depends directly on the ability to show its element type `a`.

### Recursion, corecursion, and codata

In most programming languages, a typical implementation of the factorial function looks something like this:

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

To call a function from itself in this way is not possible. Instead, recursion must be accomplished through a pattern know as a *fold*. A fold (or *catamorphism*) is a way to deconstruct data, layer by layer. It abstracts the notion of a structurally recursive computation over some algebraic data type. 

#### Fold syntax

In Coal, `fold` is a language keyword, not an ordinary function. Syntactically, it is similar to a `match` expression (explained [here](#pattern-matching)), but with one crucial difference: a `fold` carries built-in support for recursion. 

To implement the factorial function using a fold, we are going to use the `nat` data type, which [defines the natural numbers](#natural-numbers) recursively:

```
Zero, Succ(Zero), Succ(Succ(Zero)), ...
```

It is defined as:

```
type nat 
  = Zero 
  | Succ(nat)
```

This type is recursive because `nat` appears inside one of its own constructors. We mark this recursive position with the special symbol `@`:

```
type nat 
  = Zero 
  | Succ(@)
```

This location is significant. It is precisely where the `fold` mechanism will recurse. Using the special `@`-pattern syntax only available in `fold` expressions, we can now express the factorial function as:

```
  fun factorial(n : nat) =
    fold(n) {
      | Zero =>
          1
      | Succ(@p) as m =>
          m * p
    }
```

Here is how to unpack the meaning of this:

- In the base case `Zero`, the result is simply `1`.
- In the recursive case `Succ(@p) as m`:
  - `m` is bound to the current value being matched (e.g., `Succ(Succ(Zero))`),
  - `p` is bound to the result of recursively folding over the inner value — the one inside the constructor (`Succ`).

So intuitively, `@p` behaves like “the result of recursively applying this same fold to the inner structure.” In other words, the compiler performs the recursion for you.

This produces the same behavior as if you could have written an explicitly recursive definition such as:

```
      | Succ(r) => Succ(r) * fold(r)
```

but without referring to the function by name.

##### Well-foundedness

To ensure that recursion is well-founded (guaranteed to terminate), the use of `@`-patterns is restricted. Most importantly, they can only appear inside constructors. The reason for this is that a constructor’s fields are always *structurally smaller* than the value being folded. Progress toward the base case is thereby guaranteed in each step.

The following, for example, is invalid:

```
    fold(n) {
      | @p => p
    }
```

Here, `@p` appears at the top level, and not inside a constructor. This means that the fold would have no smaller sub-structure to recurse into.

##### Beyond factorial

Folds can express a wide range of recursive computations over algebraic data types. For example, here is the implementation of `reduce` for lists:

```
  fun reduce(f, acc, list) =
    fold(list, acc) {
      x :: @rec =>
        fn(a) => rec(f(x, a))
      [] =>
        fn(a) => a
    }
```

This definition captures the standard way of consuming a list by repeatedly applying a function (`f`) to its elements and an accumulator. The recursive descent through the list happens implicitly — the programmer specifies only what to do at each layer.

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

#### Codata and unfold

Recursion over ordinary data in Coal (or any language with well-founded recursion) is always guaranteed to terminate. This ensures that all data is finite as well. In many cases, this is desirable &mdash; it makes reasoning about programs predictable and safe. However, there are situations where we want potentially infinite structures or non-terminating behavior. For example:

- Infinite sequences of numbers, like the natural numbers, are easy to define in Haskell using laziness:

  ```
  nats = [0..]
  ```

- Programs that continuously run in the background, such as web servers or event loops, inherently involve non-terminating processes.

In Coal, ordinary data cannot be infinite: a `List`, `Tree`, or any recursive data type must eventually reach a base case. To express potentially infinite or ongoing computations, Coal provides a separate mechanism called *codata*.

##### Data on demand

The key difference between data and codata lies in how values are produced and consumed. Whereas data is finite and *constructed*, codata is potentially infinite and *observed*: you unfold it step by step. The following table gives a comparison between the two:

|                    | Access pattern         | Structure             | Evaluation strategy  | Invariant               |
| ------------------ | -----------------------| --------------------- | -------------------- | ----------------------- |
| **Data**           | Recursion (`fold`)     | Always finite         | Eager (strict)       | Progress                |
| **Codata**         | Corecursion (`unfold`) | Potentially infinite  | Lazy (non-strict)    | Productivity            |

Codata is ideal for representing streams, event sequences, or any ongoing process, where you only need to observe a finite part at a time. 

A codata type is introduced using the `cotype` keyword and (like a record type) is defined by a set of comma-separated fields enclosed in curly braces:

```
cotype <Name> = { <Field_1> : <t_1>, ..., <Field_n> : <t_n> }
```

Unlike records, the codata field labels start with an **uppercase** letter.

##### A basic counter

A simple codata type is a counter, which represents an infinite sequence of integers:

```
cotype Counter = { Current : int32, Next : Counter }
```

This definition involves two codata fields: `Current` gives access to the current value, and `Next` produces the next rendition of the counter. The corecursive counterpart of `fold` is `unfold`. To define a counter based on the `Counter` codata type, we can write:

```
  unfold count_from(n : int32) : Counter {
    , Current = n
    , @Next = n + 1
  }
```

Here, the `@` symbol resurfaces, but this time in the name of the field. In this context, `@Next` means that the value for `Next` is obtained corecursively, by invoking `count_from` again with the field value (in this case, `n + 1`). Conceptually, the result is equivalent to writing the following, if explicit recursion were possible:

```
  unfold count_from(n : int32) : Counter {
    , Current = n
    , Next = count_from(n + 1)
  }
```

We can now observe the counter, by accessing its fields:

```
let counter = count_from(10)

counter.Current             // => 10
counter.Next.Current        // => 11
counter.Next.Next.Current   // => 12
```

Each observation reveals one additional layer of the codata structure, producing a value that can itself be further observed. Unlike ordinary data, this can continue indefinitely &mdash; you can keep asking for `Next` without ever reaching a base case.

It is also possible to define operations that transform counters while preserving their infinite, coinductive structure:

```
  unfold transform_counter(f : int32 -> int32, c : Counter) : Counter {
    , Current = f(c.Current)
    , @Next = (f, c.Next)
  }
```



--

Aside: You may wonder, why can't we simply write `counter(n + 1)` then? Well, the reason is similar to that of folds. But instead of being concerned with progress in each step, here we are worried about *productivity*. 
Consider what would happen if we could write, for example:

```
  unfold counter(n : int32) : Counter {
    , Current = n
    , Next = counter(n + 1).Current
  }
```

Example: pseudo-randomness






--

```
  fun increment(counter : Counter) =
    counter.Next
```

```
  fun main() {
    trace_int32(counter(1).Current)
  }
```

##### Does codata have to be infinite?

#### Duality

Data and codata can be seen as two sides of the same coin. This *duality* goes deeper than mere superficial resemblance.
The idea originates in category theory, where folds and unfolds have very precise meanings.
An algebraic data type can be seen as the [initial algebra](https://en.wikipedia.org/wiki/Initial_algebra) of a functor: it provides the smallest, well-founded solution that can be consumed by a fold (a catamorphism).
In the other direction, a codata type corresponds to the [final coalgebra](https://en.wikipedia.org/wiki/Initial_algebra#Final_coalgebra) of a functor: it is the largest (potentially infinite) solution that can be observed or generated by an unfold (anamorphism).
In this description, algebras and coalgebras are mirror images: by simply reversing the direction of the arrows in their diagrams, an algebra turns into a coalgebra and vice versa.

duality is a recurring theme in category theory.

A disjoint union is sometimes referred to as a *coproduct*. The reason for this is

## Installation and setup

The compiler has been tested on Linux and Mac OS.

### Prerequisites

#### Haskell/GHC

A recent version of [GHC](https://www.haskell.org/ghc/) is needed. It is **recommended** to install Haskell, GHC and Stack using the [GHCup](https://www.haskell.org/ghcup/) tool.

#### LLVM

An [LLVM](https://llvm.org/) toolchain that provides `llc` (the LLVM static compiler) is also needed.

##### Linux

- Debian/Ubuntu (or derivatives):

  ```
  sudo apt update
  sudo apt install llvm clang     
  ```

- Fedora, RHEL, or CentOS:

  ```
  sudo dnf install llvm clang
  ```

- Arch Linux:

  ```
  sudo pacman -S llvm
  ```

##### Mac OS

See [Getting Started with the LLVM System](https://llvm.org/docs/GettingStarted.html), or install using Homebrew:

```
brew install llvm
```

**Note:** If you use Homebrew to install LLVM, you may need to add the binaries to your `PATH` manually. 

#### Additional dependencies

- GCC (probably not needed on Mac)
- [Boehm–Demers–Weiser garbage collector](https://github.com/ivmai/bdwgc)
- [The GNU Multiple Precision Arithmetic Library](https://gmplib.org/)

##### Linux

- Debian/Ubuntu (or derivatives):

  ```
  sudo apt update
  sudo apt install libgc-dev libgmp-dev build-essential
  ```

- Fedora, RHEL, or CentOS:

  ```
  sudo dnf install gc-devel gmp-devel gcc make
  ```

- Arch Linux:

  ```
  sudo pacman -S gc gmp base-devel
  ```

##### Mac OS

```
brew install bdw-gc gmp
```

### Compiler

TODO 

<!--
```
                     +---------------------------------------+                
                     |                                       |
  +--------------+   |   +------------+       +----------+   |   +--------------+
  |              |  >>>  |   Kernel   |  >>>  |   LLVM   |  >>>  |  Executable  |
  +--------------+   |   +------------+       +----------+   |   +--------------+
                     |                                       |
                     +---------------------------------------+
```
-->

## How to contribute

TODO

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
      else n * fact(n - 1) // <-- This doesn’t work
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

This is pseudo-code since letrec doesn’t really exist as a language keyword. 

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
