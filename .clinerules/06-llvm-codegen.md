# LLVM code generation

`src/Coal/Kernel/LLVM/Codegen.hs` is the main orchestrator. `irModule` takes all
modules and a target module, producing one `IRModule` (from `llvm-hs`).

Key modules:
- `Boxing.hs` — wrapping/unwrapping Coal values into LLVM representations
- `Constructor.hs` — lowering data constructors to LLVM structs and allocation
- `Function.hs` — function definitions, closures, application
- `Module.hs` — module-level LLVM IR construction
- `Prim.hs` — primitive operation codegen
- `Runtime.hs` / `RuntimeDefs.hs` — declarations of C runtime functions
- `Monad.hs` — the `IRCodegen` monad and its state

## Runtime interface

The runtime (`runtime/`) exposes a set of C functions that LLVM IR calls into.
These are declared in `RuntimeDefs.hs` as `extern` declarations.

Key runtime dependencies:
- **Boehm GC** (`rt_alloc`, `rt_alloc_atomic`) — all heap memory
- **GMP** — arbitrary-precision integer arithmetic

## Codegen expectations

- The kernel IR must be in ANF before codegen. The codegen assumes every
  sub-expression is an atom or a let-binding.
- Constructors must be fully saturated.
- All names must be globally unique within a module.
- The codegen produces one `IRModule` per kernel module; the linker phase
  (`PhaseLowering/Linking`) combines them.

## Debug output

The compiler supports emitting kernel IR at various stages.
Use `coal compile` with appropriate flags to inspect intermediate
representations for debugging codegen issues.