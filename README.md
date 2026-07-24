# Coal

This repository is the home of the **Coal programming language** and compiler. 

1. [About](#about)
1. [Documentation](#documentation)
1. [Installation and setup](#installation-and-setup)
1. [How to contribute](#how-to-contribute)
1. [License](#license)

<!-- 1. [Project status and roadmap](#project-status-and-roadmap) -->

## About

Coal is a declarative, statically typed, purely functional programming language with simple and intuitive syntax. It provides algebraic data types and pattern matching, extensible records, structural recursion, codata, and traits (type classes), among other features. Coal’s type system, like Haskell’s and ML’s, supports type inference and parametric polymorphism, drawing on the [System-F](https://en.wikipedia.org/wiki/System_F) lambda calculus. The Coal compiler is implemented in Haskell and targets [LLVM](https://llvm.org/) for code generation. As a [total](https://en.wikipedia.org/wiki/Total_functional_programming) language, Coal takes a different approach to recursion, following the motto that "[recursion is the `goto` of functional programming](https://www.semanticscholar.org/paper/Functional-Programming-with-Bananas%2C-Lenses%2C-and-Meijer-Fokkinga/5db3c6793c07285bf0f5e95fe5a25f53e7488051)." To guarantee that programs are provably terminating, recursion is only available in a restricted form, known as *structural recursion*. The language finds inspiration in ideas from the field of Mathematics of Program Construction, where streams and other infinite data types are described as [coalgebras](https://coal-lang.org/data-and-codata/) — hence the name *Coal*. 

## Documentation

The language documentation is available at: [coal-lang.org](https://coal-lang.org/)

> **Docker** :whale: 
> 
> For instructions on how to use Coal in a Docker-based workflow, please see [this page](https://codeberg.org/laserpants/coal/src/branch/main/docker#readme).

## Installation and setup

The compiler has been tested on Linux and Mac OS.

### Prerequisites

#### Haskell/GHC

A recent version of [GHC](https://www.haskell.org/ghc/) is needed. It is **recommended** to install Haskell, GHC and Stack using the [GHCup](https://www.haskell.org/ghcup/) tool.

#### LLVM

An [LLVM](https://llvm.org/) toolchain that provides `llvm-as` and `llc` (the LLVM static compiler) is also required.

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
cd coal && chmod +x project && ./project install
```

Restart or refresh your shell, using e.g., `exec $SHELL -l`. To verify that the executable is installed, run:

```
coal --version
```

#### Hello, world!

```
module Main {

  import IO(println_string)

  fun main() =
    println_string("Hello, world!")

}
```

Save this program as "Main.coal". Compile the program with the command:

```
coal compile -I. Main.coal -o dist
```
<!--

## Project status and roadmap

### Roadmap

#### Next milestone: 1

![](https://geps.dev/progress/75)
-->

<!--
| Milestone  | Feature/Fix                                           |                                                                                                                                                              
| ---------- | ----------------------------------------------------- |                                                                                                                                                              
| 1          | FFI                                                   |             

 - from_int32 -> from_integer
-->

## How to contribute

This is an open and evolving project &mdash; contributions are welcome. Please see [`CONTRIBUTING.md`](CONTRIBUTING.md) for details.

### Contributing coffee

<a href="https://www.buymeacoffee.com/laserpants"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy me a coffee" height="74" /></a>

### Documentation

The [documentation](https://coal-lang.org/) is built with Zensical. The source code is hosted at: [github.com/laserpants/coal-docs](https://github.com/laserpants/coal-docs).

## License 

This project is licensed under the terms of the MIT license. See the `LICENSE` file in this repository for details.

