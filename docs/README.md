# Coal

This repository is the home of the Coal programming language and compiler. The project is under mega-construction. 👷

## About

Coal is a purely functional, total programming language with

- simple and intuitive syntax, 
- algebraic data types, 
- rich pattern matching capabilities,
- extensible records, 
- codata, 
- traits (type classes), and 
- effect handlers (work in progress)

&hellip; among other features. Coal's type system supports type inference and parametric polymorphism, similar to Haskell, ML, and other languages based on the [System-F](https://en.wikipedia.org/wiki/System_F) lambda calculus. The Coal compiler is implemented in Haskell and targets LLVM for code generation.

### Rethinking recursion

Since Coal is a [total](https://en.wikipedia.org/wiki/Total_functional_programming) language, it takes a different approach to recursion, following the motto that "recursion is the GOTO of functional programming." To ensure that programs are provably terminating, recursion is only permitted in a restricted form, known as *structural recursion*. In this paradigm, each recursive call operates on a strictly smaller part of some finite data structure, progressing toward a base case. 

```
  fun sum(numbers : List<int32>) : int32 =
    fold(numbers) {
      | [] => 0 
      | x :: @sum => x + sum
    }
```

### Programs = Expressions + Effects

Purely functional programming is declarative and [expression-oriented](https://en.wikipedia.org/wiki/Expression-oriented_programming_language): a program is, at its core, just an expression that evaluates to a value. In this model, there are no observable side effects, no explicit mutable state, and all data is immutable. These properties make programs more predictable, easier to reason about, highly testable, and allows for code to be verified using formal mathematical methods. On the other hand, programs need to have the ability to interact with the outside world. Side-effects are what make them useful.

TODO

## Project status and roadmap

TODO

## Installation and setup

TODO

### Prerequisites

TODO

### Compiler

TODO

## How to contribute

TODO

## Language overview

TODO

### Syntax

TODO

### Modules and imports

Programs in Coal are organized as collections of modules. Modules provide a way to group related functionality into distinct namespaces.  Each module is typically focused on a specific purpose within a library or application.  A module can contain functions, type definitions, traits, and other language constructs, defined together in a single file.

```
module MerkleTree {
  // ... code  
}
```

### Language constructs

TODO

#### Control flow

TODO

##### If-then-else

TODO

### Types

#### Built-in types

Coal provides the following built-in basic language types:

| Type               | Description                             | Example values                    |                       
| ------------------ | --------------------------------------- | ------------------------- |                       
| `bool`             | Booleans                                | `true` \| `false`         |                       
| `char`             | A single Unicode character              | `'a'`, `'b'`, `'🤖'`, ... |                        
| `float`            | Single precision floating point numbers | `3.1519f`                 |                        
| `double`           | Double precision floating point numbers | `3.1519`                  |                        
| `int32`            | 32-bit integers                         | `0`, `1`, `2`, `3`, ...   |                        
| `int64`            | 64-bit integers                         | `0`, `1`, `2`, `3`, ...   |                        
| `bignum`           | Arbitrary precision integers            | `0`, `1`, `2`, `3`, ...   |                        
| `string`           | UTF-8 text string                       |  `"Hello, ✨ world!"`      |                        
| `unit`             | Singleton type                          | `()`                      |                        
| `void`             | The uninhabited type                    |                           |                        
| `nat`              | Natural numbers (Peano construction)    | `Zero`, `Succ(Zero)`, ... |                        

##### Integral types

#### Algebraic data types

TODO

### Pattern matching

TODO

### Recursion and corecursion

TODO

## License 

TODO

## Code of conduct

TODO


---
---
---
---


## About

## Rethinking recursion

In most languages, a typical (recursive) implementation of the factorial 
function looks something like the following:

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

Structural recursion works with recursive data structures like lists, trees, or other algebraic data types. This pattern is known as a *fold*. A common example is where an array of numbers is reduced into a single value, for example by adding

```
let sum = reduce(fn(n, a) => f + a, [1, 2, 3])
```

Folding doesn't work with an integers, at least not those of the machine variety. But we can define one that works.  in fact, the natural numbers are an example of the most basic inductievly defined type.

> Every natural number is either zero or the successor of another natural number.

This is known as the Peano construction of the natural numbers, named after the Italian mathematician [Giuseppe Peano](https://en.wikipedia.org/wiki/Giuseppe_Peano).
Here is how we express this as an algebraic data type:

```
type nat
 = Zero
 | Succ(nat)
```

Here is how we use the `nat` data type to define the factorial function:

```
  fun factorial(n : nat) =
    fold(n) {
      | Zero =>
          1
      | Succ(@p) as m =>
          m * p
    }
```

Note that `fold` is a language keyword in Coal, not an ordinary function.

> Why is the function rejected?

```
  fun factorial(n : int32) =
    product(enum_to(n))  // product of numbers 1, 2, ..., n

```

A strict distinction is made between finite data, which is produced and 
consumed in this way, and data that that we treat as potentially infinite. The latter is 
known is *codata*. The codata equivalents of lists, for example,  are streams.

```
  type List<a> 
    = Nil 
    | Cons(a, List<a>)
```

```
  cotype Stream<a> = 
    { Head : a
    , Tail : Stream<a> 
    }
```

The opposite ...

|                    | Access pattern        | Structure             | Evaluation strategy  |
| ------------------ | ----------------------| --------------------- | -------------------- |
| **Data**           | Recursion (fold)      | Always finite         | Eager (strict)       |
| **Codata**         | Corecursion (unfold)  | Potentially infinite  | Lazy (non-strict)    | 

## Language overview

### Built-in types

Coal provides the following built-in basic language types:

| Type               | Description                             | Values                    |                       
| ------------------ | --------------------------------------- | ------------------------- |                       
| `bool`             | Booleans                                | `true` \| `false`         |                       
| `char`             | A single Unicode character              | `'a'`, `'b'`, `'🤖'`, ... |                        
| `float`            | Single precision floating point numbers | `3.1519f`                 |                        
| `double`           | Double precision floating point numbers | `3.1519`                  |                        
| `int32`            | 32-bit integers                         | `0`, `1`, `2`, `3`, ...   |                        
| `int64`            | 64-bit integers                         | `0`, `1`, `2`, `3`, ...   |                        
| `bignum`           | Arbitrary precision integers            | `0`, `1`, `2`, `3`, ...   |                        
| `string`           | UTF-8 string values                     | `"Hello, ✨ world!"`      |                        
| `unit`             | Singleton type                          | `()`                      |                        
| `void`             | The uninhabited type                    |                           |                        
| `nat`              | Natural numbers (Peano construction)    | `Zero`, `Succ(Zero)`, ... |                        

#### Integral types

### Algebraic data types

User-defined data types are introduced with the `type` keyword.

- A *product* type combines multiple fields into a single value: All of the specified components are present together (e.g. an RGB color value that contains separate red, green, and blue components).

   ```
   type Color = Rgb(int8, int8, int8)
   ```

- A *sum* type represents a choice between alternatives: A value belongs to exactly one of the specified variants (e.g. a shape that can be either a `Circle` or a `Rectangle`).

   ```
   type Shape = Circle | Rectangle
   ```

More interesting types can be built from combinations of product and sum constructors. Here is a type that defines a binary tree, parameterized by the type (`a`) of its nodes:

```
type Tree<a> 
  = Leaf
  | Node(a, Tree<a>, Tree<a>)
```

```
          (4)
       ---------
       /       \
     (2)       (6)
    -----     -----
    /   \     /   \ 
  (1)   (3) (5)   (7)  
```

Here is how this tree is encoded with the `Tree` data type:

```
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

```
  type JsonValue
    = JsonNull
    | JsonBool(bool)
    | JsonNumber(double)
    | JsonString(string)
    | JsonArray(List<JsonValue>)
    | JsonObject(List<(string, JsonValue)>)
```

### List, Option, etc.

A list is an ordered collection of elements where all entries are of the same type. 
It is a foundational data structure in functional programming, commonly used to store and manipulate collections of data. 
In Coal, list literals are denoted by a sequence of comma-separated expressions, enclosed in square brackets:

```
[<expr_1 : t>, <expr_2 : t>, ..., <expr_n : t>] : List<t>
```

The `List` type is defined inductively, and implemented as a one-way *linked list* of nodes. 
This means that a list of type `List<a>` is either:

- The empty list; or
- A value of type `a`, coupled with another `List<a>` list. 

These last two are referred to as the *head* and *tail* of the list. 

```
type List<a>
  = []
  | a :: List<a>
```

```
type Option<a>
  = Some(a)
  | None
```

### Tuples

Just like lists, tuples are ordered sequences of values. Unlike lists, however:

1. A tuple's length is fixed (i.e. determined at compile-time), and
2. Its elements may have different types.

In code, a tuple is written as a comma-separated sequence of expressions enclosed in parentheses:

```
(<expr_1>, <expr_2>, ..., <expr_n>)
```

#### Examples:

```
(10, "covfefe", false)
```

The type of the tuple in this example is: 

```
(int32, string, bool)
```

Tuples of length two and three are often called *pairs* and *triples*, respectively. 
There is no singleton tuple type. A single value in parentheses is just the value itself:

```
(42)  // Not a tuple, just the integer 42
```

The empty tuple *does* exist. It is written `()` and is known as the unit value. Its type is `unit`. (See *Built-in types* for more details.)

```
()            : unit                           // unit value
(1, 2)        : (int32, int32)                 // 2-tuple
(1, 2, 3)     : (int32, int32, int32)          // 3-tuple
(1, 2, 3, 4)  : (int32, int32, int32, int32)   // 4-tuple
// ...
```

#### Tuples and currying

It is important to distinguish between a function that takes multiple arguments and one that takes a single tuple as its argument.

Consider the two following function signatures:

```
f : a -> b -> c
g : (a, b) -> c
```

Note that `f` and `g` have incompatible types.
The first is known as a *curried* function type, and it is usually more convenient to work with. 
Curried functions can be partially applied, which is useful, for example, when working with higher-order functions. 
If you really intend to specify a tuple as the only argument to a function, you need to use an extra pair of parentheses:

```
fun add((a, b)) = a + b

let five = add((1, 4))
```

The `curry` and `uncurry` combinators convert an uncurried function into a curried one, and vice versa.

```
curry : ((a, b) -> c) -> a -> b -> c
uncurry : (a -> b -> c) -> (a, b) -> c
```

Here is how `curry` is used with the uncurried function `add` to change it into curried form.

```
let five = curry(add, 1, 4)         // or (curry(add))(1, 4)
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

### Records

Records are unordered collections of name-value pairs, where the values can be of arbitrary type, including other records. 
They are suitable for representing structured data with multiple properties, and nested objects. 
A record is written as a sequence of comma-separated fields enclosed in curly braces. 
Each field consists of a name, referred to as the *label*, paired with a value. These two are separated by an equals sign (`=`). 

```
{ 
  name = "Eros Ramazzotti", 
  shoe_size = 43, 
  privileges = ["read", "edit", "karaoke"]
}
```

```
{ name : string, shoe_size : int32, privileges : List<string> }
```

### Expression syntax

### Traits

A trait describes a collection of functions that must be defined for the 
underlying type. Traits in Coal are similar to type classes in Haskell. A common analogy is to think of them as interfaces in object-oriented programming. 


```
trait Functor<f : * -> *> {
  map : (a -> b) -> f<a> -> f<b>
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

### Modules

Programs in Coal are organized into modules. Modules provide a way to group related functionality into distinct namespaces.
Each module is typically focused on a specific purpose within a library or application.
A module can contain functions, type definitions, traits, and other language constructs, defined together in a single file.

```
module MerkleTree {
  // ... code  
}
```

## Effects as a side business

> Programs = Expressions + Effects

Purely functional programming is declarative and [expression-oriented](https://en.wikipedia.org/wiki/Expression-oriented_programming_language): a program is, at its core, just an expression that evaluates to a value. In this model, there are no observable side effects, no explicit mutable state, and all data is immutable. These properties make programs more predictable, easier to reason about, highly testable, and allows for code to be verified using formal mathematical methods. On the other hand, programs need to have the ability to interact with the outside world. Side-effects are what make them useful.


Purely functional programming is declarative and [expression-oriented](https://en.wikipedia.org/wiki/Expression-oriented_programming_language). 
A program is, in essence, just an expression that evaluates to a value.
There are no observable side-effects, no *explicit state*, and all data is immutable. This leads to more predictable program behavior, makes the code easier to reason about, improves testability, and allows for code to be verified using formal mathematical methods. On the other hand, programs need to have the ability to interact with the outside world. Side-effects are what make them useful.

Similar to how the user interface describes
is a 
we can think of an effect boundary as similar to a user interface — but instead 
of mediating between a human and a program, it mediates between pure logic and 
the external world. Pure code produces structured descriptions of effects, and 
effect handlers at the boundary interpret those descriptions to perform real-world actions.



<!--

### Project status and roadmap

### Overview

#### Built-in types

##### Language primitives

| Type               | Description                             | Values                    |                       
| ------------------ | --------------------------------------- | ------------------------- |                       
| `bool`             | Booleans                                | `true` \| `false`         |                       
| `char`             | A single Unicode character              | `'a'`, `'b'`, `'🤖'`, ... |                        
| `float`            | Single precision floating point numbers | `3.1519f`                 |                        
| `double`           | Double precision floating point numbers | `3.1519`                  |                        
| `int32`            |                                         | `0`, `1`, `2`, `3`, ...   |                        
| `int64`            |                                         | `0`, `1`, `2`, `3`, ...   |                        
| `bignum`           | Arbitrary precision integers            | `0`, `1`, `2`, `3`, ...   |                        
| `string`           |                                         | `"Hello, ✨ world!"`      |                        
| `unit`             |                                         | `()`                      |                        
| `void`             | The uninhabited type                    |                           |                        
| `nat`              | Natural numbers                         | `Zero`, `Succ(Zero)`, ... |                        


##### The unit type

The unit value is written as an empty pair of parentheses. It is the 
one-and-only value of type `unit`. 

```
() : unit
```

#### Algebraic data types

User-defined data types are introduced with the `type` keyword.


```
<type_definition>       ::= "type" <type_name> [ "(" <type_parameters> ")" ] 
                            "=" <constructor> { "|" <constructor> }

<constructor>           ::= <constructor_name> [ "(" <constructor_arguments> ")" ]
<constructor_arguments> ::= <type_expression> { "," <type_expression> }
<type_expression>       ::= <basic_type> | <type_name> [ "(" <type_arguments> ")" ]
<type_arguments>        ::= <type_expression> { "," <type_expression> }
<type_parameters>       ::= <type_parameter> { "," <type_parameter> }
<type_name>             ::= <uppercase_identifier>
<type_parameter>        ::= <lowercase_identifier>
<constructor_name>      ::= <uppercase_identifier>
```

Here is a type that defines a binary tree, parameterized by the type (`a`) of
its nodes.

```
type Tree(a) 
  = Leaf
  | Node(a, Tree(a), Tree(a))
```

adsfdsf

```
          (4)
       ---------
       /       \
     (2)       (6)
    -----     -----
    /   \     /   \ 
  (1)   (3) (5)   (7)  
```

Here is how this tree is encoded with the Tree data type:

```
my_tree = Node(4, 
  Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)), 
  Node(6, Node(5, Leaf, Leaf), Node(7, Leaf, Leaf)))
```

#### Pattern matching

#### Records

Records are suitable for representing structured data with multiple properties, and nested objects: 

```
user =
  { id = 99
  , name = "Obi-Wan Kenobi"
  , permissions = ["read", "write", "karaoke"]
  }
```

a

```
user : { name : string, id : int32, permissions : List(string) }
```

#### Traits

A trait describes a collection of functions that must be defined for the 
underlying type. Traits are analogous to type classes in Haskell. 


```
trait Functor(f : * -> *) {
  map : (a -> b) -> f(a) -> f(b)
}
```

aa

```
type Option(a)
  = Some(a)
  | None
```

```
// Make Option an instance of the Functor trait
instance Functor(Option) {
  map(f, opt) =
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

#### Side-effects

Code in purely functional programming has no observable side-effects. This has the advantage that programs can 
be reasoned about and verified formally, using mathematical proof techniques.

be reasoned about equationally, and verified using formal mathematical proof techniques. 
examples...
On the other hand, it 
I/O monad

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

asfd

```
let fact = 
  fn(n) => 
    if (n == 0)
      then 1 
      else n * fact(n - 1) // <-- This doesn't work
  in fact(5)
```

In OCaml, for example, this same rule applies with regards to the default `let` 
construct, but this can easily be overridden using the `let rec` keyword. As an
aside, Coal actually has a recursive let binding too, but it is only available
to the compiler.

A function defined at the top-level is (technically) a let-binding, which means that the function itself is not in scope inside the function body:

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
