---
applyTo:
  - "**/*.coal"
---

# Coal Language Syntax Guide

This guide provides comprehensive reference for Coal language syntax, designed for AI agents working with Coal code. Coal is a statically typed, purely functional language with Haskell/ML-inspired type system.

## Critical Distinctions from Other Functional Languages

**Coal is NOT Haskell/ML/OCaml**. Key differences:

1. **Function application uses parentheses and commas** (like C/Java/Python):

   ```coal
   concat("one", "two")        // Coal ✓
   concat "one" "two"          // Haskell/ML ✗
   ```

2. **No explicit recursion** — functions cannot call themselves by name:

   ```coal
   fun fib(n) = fib(n-1) + fib(n-2)  // ✗ ERROR: Explicit recursion detected
   ```

3. **No `let rec`** — let-bindings are NOT recursive (unlike OCaml):

   ```coal
   let x = x + 1 in x  // ✗ ERROR: Name "x" not in scope in definition
   ```

4. **Recursion via `fold` keyword** with `@`-patterns:

   ```coal
   fun factorial(n : nat) =
     fold(n) {
       | Zero => 1
       | Succ(@p) as m => m * p  // @ binds to recursive result
     }
   ```

5. **Shadowing is an error** (unlike most languages):
   ```coal
   fun f(x) = let x = 3 in x  // ✗ ERROR: Shadowing not allowed
   ```

## Reserved Keywords

**NEVER use these as variable names:**

```
alias       float       int64       true
as          fn          let         type
bignum      fold        match       unit
bool        fun         module      when
char        if          nat         where
double      import      or          with
do          in          string
else        instance    then
false       int32       trait
```

## Module System

### Module declaration

```coal
module %Path(%export_list) {
  %import_statement
  %definition
  ...
}
```

- Path mirrors directory structure: `Utils.Math.Trigonometry` → `Utils/Math/Trigonometry.coal`
- Path segments start with uppercase, separated by dots
- Export list is optional; omit to export everything

### Imports

```coal
import List(concat, head, tail)              // Import specific functions
import Utilities(Answer(Yes, No))            // Import type with constructors
import Utilities(Answer)                     // Import type with all constructors
import namespace List                        // Qualified import: use List.concat
import Utilities(count)                      // Import trait method directly
```

### Exports

```coal
module Utils.Math(sin, cos, tan) { ... }     // Only export listed names
module Utils.Math { ... }                    // Export everything
```

## Type System

### Built-in Types

| Type     | Description                 | Examples                  |
| -------- | --------------------------- | ------------------------- |
| `bool`   | Booleans                    | `true`, `false`           |
| `char`   | Unicode character           | `'a'`, `'🤖'`             |
| `float`  | Single precision float      | `3.14f`                   |
| `double` | Double precision float      | `3.141592653589793`       |
| `int32`  | 32-bit integer              | `42`, `-100`              |
| `int64`  | 64-bit integer              | `9000000000`              |
| `bignum` | Arbitrary precision integer | `12345678901234567890`    |
| `string` | UTF-8 text                  | `"Hello, world!"`         |
| `unit`   | Singleton type              | `()`                      |
| `void`   | Uninhabited type            | (no values)               |
| `nat`    | Natural numbers (Peano)     | `Zero`, `Succ(Zero)`, ... |

### Natural Numbers (`nat`)

Recursive definition enabling pattern matching on numbers:

```coal
type nat = Zero | Succ(nat)
```

Conversion functions (O(1) operations):

```coal
pack   : int32 -> nat    // Convert int32 to nat
unpack : nat -> int32    // Convert nat to int32
```

Example:

```coal
Succ(Succ(Succ(Zero)))  // The number 3
pack(5)                 // Efficient way to write Succ(Succ(Succ(Succ(Succ(Zero)))))
```

### Numeric Literal Overloading

Integer literals are polymorphic:

```coal
let a : int32 = 100    // 100 inferred as int32
let b : bignum = 100   // 100 inferred as bignum
let c : double = 100   // 100 inferred as double (becomes 100.0)
```

Float literals require `f` suffix:

```coal
3.0    // double
3.0f   // float
```

### Function Types

Arrow notation (right-associative):

```coal
a -> b -> c  ≡  a -> (b -> c)    // Curried function
(a, b) -> c                      // Function taking tuple
```

### Type Aliases

```coal
type alias User = { username : string, email : string }
type alias Dictionary<val> = Map<string, val>
```

### Algebraic Data Types

```coal
type Shape = Circle(float) | Rectangle(float, float)
type Tree<a> = Leaf | Node(a, Tree<a>, Tree<a>)
type Option<a> = Some(a) | None
type Result<r, e> = Ok(r) | Err(e)
```

### Records

#### Closed records (exact fields)

```coal
{ name : string, age : int32 }
```

#### Open records (extensible)

```coal
{ lat : float, lng : float | r }  // Has lat/lng plus whatever is in row variable r
{ | r }                           // Any record (row variable captures all fields)
```

#### Record operations

```coal
// Field access
person.name

// Extension
{ tag = "new" | rec }

// Pattern matching
fun get_name({ name = n | _ }) = n
fun drop_name({ name = _ | fields }) = fields

// Shorthand syntax
fun full_name({ first_name, last_name }) = first_name +++ " " +++ last_name
```

### Tuples

```coal
()                    : unit
(1, 2)                : (int32, int32)
(1, "two", true)      : (int32, string, bool)
```

**Note:** Single-element in parentheses is NOT a tuple:

```coal
(42)  // Just the integer 42, not a tuple
```

### Lists

```coal
[1, 2, 3, 4]          : List<int32>
[]                    : List<a>
x :: xs               // Cons operator (prepend x to xs)
```

List pattern matching:

```coal
match(list) {
  | [] => "empty"
  | head :: tail => "non-empty"
  | [a, b, c] => "exactly three elements"
}
```

### Type Annotations

```coal
let x : int32 = 42
fun process(value : string) = ...
fun process(value : string) : return_type = ...
```

## Expressions

### Variables

**Naming rules:**

- Start with lowercase letter or underscore
- Can contain letters, digits, underscores
- Case-sensitive
- Cannot be reserved keywords
- **No shadowing allowed** (this is an error in Coal)

### Function Application

```coal
concat("one", "two")              // Apply function to arguments
add(1)                            // Partial application (currying)
map(add(1), [1, 2, 3])            // Partially applied function as argument
```

### Lambda Expressions

```coal
fn(x) => x + 1
fn(x, y) => x + y
fn((lhs, rhs)) => lhs             // Pattern in lambda argument
```

### Let-bindings

**Expression-level:**

```coal
let x = 1; y = 2 in x + y
let (a, b) = (1, 2) in a + b      // Pattern binding
```

**Function binding syntax:**

```coal
let add(x, y) = x + y in add(1, 2)
// Equivalent to:
let add = fn(x, y) => x + y in add(1, 2)
```

**CRITICAL:** Let-bindings are NOT recursive:

```coal
let x = x + 1 in x  // ✗ ERROR: x not in scope in its own definition
```

### If-then-else

```coal
if (condition) then expr1 else expr2
```

Both branches required and must have same type.

### Match Expressions

```coal
match(value) {
  | pattern1 => expr1
  | pattern2 => expr2
  | _ => default
}
```

**Must be exhaustive** (cover all cases).

### Guards

```coal
match(parse_int32(s)) {
  | Some(n) when (n == target) => "correct"
  | Some(n) when (n > target) => "too high"
  | Some(n) otherwise => "too low"
  | None => "not a number"
}
```

### Lambda Match

```coal
match {
  | [] => true
  | _ => false
}
// Equivalent to:
fn(val) => match(val) { | [] => true | _ => false }
```

### Type Annotations in Expressions

```coal
(42 : int32)
(x : string) +++ " world"
```

## Pattern Matching

### Supported Patterns

| Pattern Type     | Example              | Description                                   |
| ---------------- | -------------------- | --------------------------------------------- |
| Constructor      | `Color(r, g, b)`     | Match data constructor                        |
| Variable         | `x`                  | Match anything, bind to variable              |
| Wildcard         | `_`                  | Match anything, ignore value                  |
| Literal          | `"hello"`, `0`, `()` | Match exact literal value                     |
| List constructor | `x :: xs`            | Match head and tail                           |
| List literal     | `[a, b, c]`          | Match exact length list                       |
| Tuple            | `(fst, snd)`         | Match tuple components                        |
| Record           | `{ name = n \| _ }`  | Match record fields                           |
| As-pattern       | `(x, _) as pair`     | Bind both inner pattern and whole value       |
| **@-pattern**    | `Succ(@n)`           | **Fold recursion** (only in fold expressions) |
| Or-pattern       | `1 or 2`             | Match either alternative                      |

### @-Patterns (Fold Recursion)

**Only valid inside `fold` expressions:**

```coal
fold(n) {
  | Zero => 1
  | Succ(@p) as m => m * p
}
```

- `@p` binds to the **result of recursively folding** the inner value
- `m` binds to the current value being matched
- **Cannot** appear at top level or outside constructors

Invalid:

```coal
fold(n) {
  | @p => p  // ✗ ERROR: @ must be inside constructor
}
```

## Operators

### Arithmetic

| Op  | Description    | Type                                 |
| --- | -------------- | ------------------------------------ |
| `+` | Addition       | `∀n : n -> n -> n with Numeric(n)`   |
| `-` | Subtraction    | `∀n : n -> n -> n with Numeric(n)`   |
| `*` | Multiplication | `∀n : n -> n -> n with Numeric(n)`   |
| `/` | Division       | `∀q : q -> q -> q with Divisible(q)` |
| `^` | Exponentiation | `∀n : n -> nat -> n with Numeric(n)` |
| `%` | Modulus        | `∀m : m -> m -> m with Modulo(m)`    |

### Comparison

| Op   | Description           | Type                                     |
| ---- | --------------------- | ---------------------------------------- |
| `==` | Equality              | `∀n : n -> n -> bool with Comparable(n)` |
| `!=` | Inequality            | `∀n : n -> n -> bool with Comparable(n)` |
| `<`  | Less than             | `∀n : n -> n -> bool with Ordered(n)`    |
| `>`  | Greater than          | `∀n : n -> n -> bool with Ordered(n)`    |
| `<=` | Less than or equal    | `∀n : n -> n -> bool with Ordered(n)`    |
| `>=` | Greater than or equal | `∀n : n -> n -> bool with Ordered(n)`    |

### Logical

| Op     | Description | Type                   | Notes          |
| ------ | ----------- | ---------------------- | -------------- |
| `&&`   | AND         | `bool -> bool -> bool` | Short-circuits |
| `\|\|` | OR          | `bool -> bool -> bool` | Short-circuits |
| `!`    | NOT         | `bool -> bool`         | Unary operator |

### String

| Op    | Description          | Type                         |
| ----- | -------------------- | ---------------------------- |
| `+++` | String concatenation | `string -> string -> string` |

### List

| Op   | Description    | Type                                 |
| ---- | -------------- | ------------------------------------ |
| `++` | Concatenation  | `∀a : List<a> -> List<a> -> List<a>` |
| `::` | Cons (prepend) | `∀a : a -> List<a> -> List<a>`       |

### Function Composition

| Op    | Description         | Type                             |
| ----- | ------------------- | -------------------------------- |
| `>>`  | Forward composition | `(a -> b) -> (b -> c) -> a -> c` |
| `<<`  | Reverse composition | `(b -> c) -> (a -> b) -> a -> c` |
| `\|.` | Reverse application | `a -> (a -> b) -> b`             |
| `.\|` | Forward application | `(a -> b) -> a -> b`             |

### Other

| Op   | Description         | Type                                 |
| ---- | ------------------- | ------------------------------------ |
| `.`  | Record field access | `{ field : a \| r } -> a`            |
| `<>` | Semigroup operator  | `∀a : a -> a -> a with Semigroup(a)` |

## Recursion and Corecursion

### Structural Recursion with `fold`

**Expression-level fold:**

```coal
fun factorial(n : nat) =
  fold(n) {
    | Zero => 1
    | Succ(@p) as m => m * p
  }
```

**How it works:**

- `@p` = result of recursively folding the inner value
- `m` = current value being matched
- Compiler guarantees termination (structural recursion)

**Multi-argument fold:**

```coal
fun reduce(f, acc, list) =
  fold(list, acc) {
    | x :: @rec => fn(a) => rec(f(x, a))
    | [] => fn(a) => a
  }
```

### Top-level Folds (Mutual Recursion)

For mutually recursive data types:

```coal
fold encode_json_value : JsonValue -> string {
  | JsonNull => "null"
  | JsonArray(encode_json_array(@values)) =>
      "[" +++ intercalate(",", values) +++ "]"
  | JsonObject(encode_json_object(@pairs)) =>
      "{" +++ intercalate(",", pairs) +++ "}"
}

fold encode_json_array : List<JsonValue> -> List<string> {
  | [] => []
  | encode_json_value(@value) :: encode_json_array(@values) =>
      value :: values
}
```

### Well-foundedness Restriction

**@-patterns MUST appear inside constructors:**

```coal
fold(n) {
  | Succ(@p) => ...    // ✓ OK: @ inside constructor
  | @p => ...          // ✗ ERROR: @ at top level
}
```

This ensures each recursion step works on a structurally smaller value.

### Codata and the Process Type

**Codata = potentially infinite, lazy structures**

Unlike data (finite, constructed), codata is observed step by step:

```coal
type alias Stream<a> = Process<a, unit>
```

**Creating processes:**

```coal
fun repeat(state : a) : Stream<a> =
  process(state, fn(_, _) => state)

fun enum_from(n : int32) : Stream<int32> =
  process(n, fn(_, m) => m + 1)
```

**Observing processes:**

```coal
let numbers = enum_from(5)

numbers |.head                      // 5
numbers |.tail |.head               // 6
numbers |.tail |.tail |.head        // 7
```

**Transforming processes:**

```coal
let evens = enum_from(0) |.map_process(fn(n) => n * 2)
```

**Key functions:**

- `process : a -> (v -> a -> a) -> Process<a, v>` — Create process
- `head : Stream<a> -> a` — Observe current state
- `tail : Stream<a> -> Stream<a>` — Advance to next state
- `map_process : (a -> b) -> Process<a, v> -> Process<b, v>` — Transform observations

## Traits (Type Classes)

### Defining Traits

```coal
trait Ordered<t> {
  compare : t -> t -> Order
}
```

### Trait Instances

```coal
instance Ordered<bool> {
  fun compare(a, b) =
    match((a, b)) {
      | (false, true) => LessThan
      | (true, false) => GreaterThan
      | (_, _) => EqualTo
    }
}
```

### Using Traits (Constraints)

```coal
fun is_less_than(x : t, y : t) : bool with Ordered<t> =
  compare(x, y) == LessThan
```

Type: `∀t : t -> t -> bool with Ordered<t>`

### Built-in Traits

| Trait            | Methods                               | Purpose                     |
| ---------------- | ------------------------------------- | --------------------------- |
| `Numeric<n>`     | `+`, `-`, `*`, `negate`, `from_int32` | Arithmetic operations       |
| `Divisible<q>`   | `/`                                   | Division                    |
| `Modulo<m>`      | `%`                                   | Modulus                     |
| `Comparable<n>`  | `==`, `!=`                            | Equality testing            |
| `Ordered<n>`     | `<`, `>`, `<=`, `>=`, `compare`       | Ordering comparisons        |
| `Semigroup<a>`   | `<>`                                  | Associative binary operator |
| `Functor<f>`     | `map : (a -> b) -> f<a> -> f<b>`      | Mapping over structures     |
| `Applicative<f>` | `fmap`, `pure`                        | Applicative functors        |
| `Monad<m>`       | `bind : m<a> -> (a -> m<b>) -> m<b>`  | Monadic computations        |

### Higher-kinded Traits

```coal
trait Functor<f> {
  map : (a -> b) -> f<a> -> f<b>
}

instance Functor<Option> {
  fun map(f, opt) =
    match(opt) {
      | Some(a) => Some(f(a))
      | None => None
    }
}
```

### Trait Inheritance

```coal
trait Show<Option<a>> with Show<a> {
  fun show(opt) =
    match(opt) {
      | Some(v) => "Some(" +++ show(v) +++ ")"
      | None => "None"
    }
}
```

## IO and Effects

### IO Type

`IO<a>` represents a computation that performs side effects and produces value of type `a`.

### Common IO Functions

```coal
println_string : string -> IO<unit>
print_string   : string -> IO<unit>
println_int32  : int32 -> IO<unit>
readln         : unit -> IO<string>
read_file      : string -> IO<Result<string, FileError>>
write_file     : string -> string -> IO<unit>
random         : unit -> IO<double>
```

### Sequencing IO with `and_then`

```coal
println_string("Enter name")
  |. and_then(readln)
  |. and_then(fn(s) => println_string("Hello, " +++ s +++ "!"))
```

### Do-notation

```coal
do {
  println_string("Enter your name");
  name <- readln();
  println_string("Hello, " +++ name +++ "!");
}
```

- Statements separated by semicolons
- `<-` binds monadic result to variable
- Syntactic sugar for `and_then` chains

## Top-level Definitions

### Functions

```coal
fun name(arg1, arg2, ...) = expr
fun name(arg1 : type1, arg2 : type2) : return_type = expr
```

**Pattern-matching style:**

```coal
fun unpack
  | [a], true    => a
  | [a, _], true => a
  | _, _         => 0
```

### Main Function

```coal
module Main {
  fun main() = ...
}
```

Entry point of the program.

### Let-bindings (top-level)

```coal
let days = ["Monday", "Tuesday", "Wednesday", ...]
let days : List<string> = [...]
let add = fn(x, y) => x + y
```

**Cannot** bind to patterns at top level:

```coal
let (a, b) = (1, 2)  // ✗ ERROR: Only allowed inside expressions
```

### Data Types

```coal
type Name = Constructor1 | Constructor2(type) | ...
type Name<a, b> = ...
```

### Type Aliases

```coal
type alias Name = existing_type
type alias Name<a> = parameterized_type<a>
```

### Traits and Instances

```coal
trait Name<param> {
  method1 : type1
  method2 : type2
}

instance Name<concrete_type> {
  fun method1(...) = ...
  fun method2(...) = ...
}
```

## Common Pitfalls

1. **Explicit recursion forbidden:**

   ```coal
   fun fib(n) = fib(n-1) + fib(n-2)  // ✗ Use fold instead
   ```

2. **Let-bindings not recursive:**

   ```coal
   let x = x + 1 in x  // ✗ x not in scope
   ```

3. **Shadowing is an error:**

   ```coal
   fun f(x) = let x = 3 in x  // ✗ Cannot shadow x
   ```

4. **Function application needs parentheses:**

   ```coal
   map fn(x) => x + 1, xs  // ✗ Wrong syntax
   map(fn(x) => x + 1, xs) // ✓ Correct
   ```

5. **Match expressions must be exhaustive:**

   ```coal
   match(opt) {
     | Some(x) => x  // ✗ Missing None case
   }
   ```

6. **Unit function calls need double parentheses OR single:**
   ```coal
   fun five(()) = 5
   five(())     // Explicit unit
   five()       // Shorthand (compiler accepts this)
   ```

## Standard Library Patterns

### List Operations

```coal
import List(head, tail, length, map, filter, reduce, concat, take, drop)

head([1, 2, 3])              // Some(1)
length([1, 2, 3])            // Succ(Succ(Succ(Zero))) : nat
map(fn(x) => x * 2, [1, 2])  // [2, 4]
filter(fn(x) => x > 2, xs)   // Keep elements > 2
reduce(fn(x, a) => x + a, 0, [1, 2, 3])  // 6
```

### Option Type

```coal
import Option(with_default, map, and_then)

with_default("Anonymous", user.name)
map(fn(x) => x * 2, Some(5))  // Some(10)
```

### String Operations

```coal
import String(length, head, tail, concat, reverse, drop, cons)

"Hello" +++ " " +++ "world"   // "Hello world"
reverse("hello")               // "olleh"
length("hi")                   // Succ(Succ(Zero))
```

### Nat (Natural Numbers)

```coal
import Nat(pack, unpack)

pack(5)     // Convert int32 to nat (O(1))
unpack(n)   // Convert nat to int32 (O(1))
```

## Comments

```coal
// Single-line comment

/* Multi-line comment
   can span multiple lines
   /* and can be nested */
*/
```

## Example Programs

### Factorial with fold

```coal
module Main {
  import IO(println_int32)
  import Nat(pack, unpack)

  fun factorial(n : nat) : nat =
    fold(n) {
      | Zero => Succ(Zero)
      | Succ(@p) as m => m * p
    }

  fun main() =
    let result = factorial(pack(5))
    in println_int32(unpack(result))
}
```

### Pattern matching on lists

```coal
fun head_or_default
  | [], default    => default
  | x :: _, _      => x

fun sum(list : List<int32>) : int32 =
  fold(list) {
    | [] => 0
    | x :: @rest => x + rest
  }
```

### Using traits

```coal
instance Numeric<Complex> {
  fun `+`(Complex(r1, i1), Complex(r2, i2)) =
    Complex(r1 + r2, i1 + i2)
  fun `*`(Complex(r1, i1), Complex(r2, i2)) =
    Complex(r1 * r2 - i1 * i2, r1 * i2 + i1 * r2)
  // ... other methods
}
```

### IO with do-notation

```coal
module Main {
  import IO(println_string, readln)

  fun main() =
    do {
      println_string("What's your name?");
      name <- readln();
      println_string("Hello, " +++ name +++ "!");
    }
}
```

## Quick Reference Card

**Module:** `module Path(exports) { imports; definitions }`  
**Import:** `import Module(items)` or `import namespace Module`  
**Function:** `fun name(args) = expr` or `fun name | pattern => expr | ...`  
**Let:** `let x = expr in body` or `let x : type = expr in body`  
**Lambda:** `fn(args) => expr`  
**If:** `if (cond) then expr1 else expr2`  
**Match:** `match(value) { | pattern => expr | ... }`  
**Fold:** `fold(value) { | pattern => expr | ... }` (use `@` in patterns)  
**Type:** `type Name = Constructor1 | Constructor2(...)`  
**Alias:** `type alias Name = existing_type`  
**Trait:** `trait Name<param> { method : type }`  
**Instance:** `instance Trait<Type> { fun method(...) = ... }`  
**Record:** `{ field1 = value1, field2 = value2 }`  
**List:** `[elem1, elem2, ...]` or `head :: tail`  
**Tuple:** `(elem1, elem2, ...)`  
**String concat:** `"str1" +++ "str2"`  
**List concat:** `list1 ++ list2`  
**Pipeline:** `value |. function` (reverse application)  
**Compose:** `f >> g` (forward) or `f << g` (reverse)

---

**Remember:** Coal enforces totality. All functions must terminate. Use `fold` for recursion, `Process` for codata. No explicit recursion, no shadowing, no `let rec`.
