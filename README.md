# Coal

This repository is the home of the **Coal programming language** and compiler. 

1. [About](#about)
1. [Documentation](#documentation)
1. [Installation and setup](#installation-and-setup)
1. [Project status and roadmap](#project-status-and-roadmap)
1. [How to contribute](#how-to-contribute)
1. [License](#license)

## About

Coal is a declarative, statically typed, purely functional programming language with simple and intuitive syntax. It provides algebraic data types and pattern matching, extensible records, structural recursion, codata, corecursion, and traits (type classes), among other features. Coal’s type system, like Haskell’s and ML’s, supports type inference and parametric polymorphism, drawing on the [System-F](https://en.wikipedia.org/wiki/System_F) lambda calculus. The Coal compiler is implemented in Haskell and targets [LLVM](https://llvm.org/) for code generation. The language is based on ideas where streams and other infinite data types are described as [coalgebras](https://coal-lang.org/data-and-codata/) — hence the name *Coal*.

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

These code samples illustrate two distinct modes of recursive control flow. If you are familiar with [recursion schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html) in a language like Haskell, recursion in Coal is based on the same principles. In that framework, `fold` and `unfold` are called *catamorphisms* and *anamorphisms*, respectively. 

<!--
### Programs = Expressions + Effects

Coal is a highly [expression-oriented](https://en.wikipedia.org/wiki/Expression-oriented_programming_language) language: a program is, at its core, just an expression that evaluates to a value. In this programming model, all data is immutable and there are no observable side-effects. These properties make programs more predictable, easier to reason about, highly testable, and allows for code to be verified using formal mathematics. On the other hand, practical applications need to have the ability to interact with the outside world. Side-effects are what make them useful. A [system for managing effects](https://en.wikipedia.org/wiki/Effect_system), such as I/O and exceptions, is still lacking in Coal. This is an essential step to promote the language into one that can be used to write actual programs. See **[How to contribute](#how-to-contribute)** if you’re keen to work on this.
-->

## Documentation

The language documentation is available at: [coal-lang.org](https://coal-lang.org/)

## Installation and setup

The compiler has been tested on Linux and Mac OS.

### Prerequisites

#### Haskell/GHC

A recent version of [GHC](https://www.haskell.org/ghc/) is needed. It is **recommended** to install Haskell, GHC and Stack using the [GHCup](https://www.haskell.org/ghcup/) tool.

#### LLVM

An [LLVM](https://llvm.org/) toolchain that provides `llc` (the LLVM static compiler) is also required.

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

### Building the compiler

Clone the repository:

```
git clone ssh://git@codeberg.org/laserpants/coal.git
```

```
cd coal && stack install
```

Restart or refresh your shell, using e.g., `exec $SHELL -l`. To verify that the executable is installed, run:

```
coal --help
```

#### Hello, world!

```
module Main {

  fun main() = trace_string("Hello, world!")

}
```

Save this program as "Main.coal". Compile the program with the command:

```
coal Main.coal -o dist
```

## Project status and roadmap

### Roadmap

<!--
#### Next milestone: 1

![](https://geps.dev/progress/75)
-->

| Milestone  | Feature/Fix                                           |                                                                                                                                                              
| ---------- | ----------------------------------------------------- |                                                                                                                                                              
| 1          | Module imports/exports                                |                                                                                      
| 1          | Error messages                                        |
| 1          | FFI                                                   |             

<!--
 - from_int32 -> from_integer
-->

## How to contribute

This is an open and evolving project &mdash; contributions are welcome.

### Bug reports 

If you want to report a bug 🐞, [open an issue](https://codeberg.org/laserpants/coal/issues) with:

- A description of the problem
- Steps to reproduce it
- What you expected to happen
- What actually happened (include error messages or stack traces)

If possible, include a small code example that demonstrates the issue — this makes debugging much easier.

### Contributing code

If you want to work on a feature or bug fix, fork the repository and create a new branch for your work. Then submit a pull request with a description of:

- What you changed
- Why the change was made
- Relevant issue number (if any)

### Documentation

The [documentation](https://coal-lang.org/) is built with MkDocs and the Material for MkDocs (mkdocs-material) theme. The source code is hosted at: [github.com/laserpants/coal-docs](https://github.com/laserpants/coal-docs).

## License 

This project is licensed under the terms of the MIT license. See the `LICENSE` file in this repository for details.
