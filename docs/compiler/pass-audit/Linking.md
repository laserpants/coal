# Linking

## Purpose

Assemble LLVM bitcode to object files via `llc`, compile the runtime C library with
`gcc`, and link everything into a final executable.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseLowering/Linking.hs
```

---

## Summary

The final pass in the compiler pipeline. Takes bitcode for all modules and produces
a native executable. Uses `llc` to compile bitcode to object files, embeds the runtime
C library (compiled from `runtime/dist/runtime-combined.c`), and links with Boehm GC
(`-lgc`) and GMP (`-lgmp`). Additional C files can be passed on the command line via
`configCFiles`.

---

## Input

- `[(Name, ByteString)]` — list of (module name, LLVM bitcode) pairs

## Output

- `()` — side effect of producing the executable file

---

## Detailed Behavior

### `compileBitcode`

1. Creates a temporary working directory
2. For each (name, bitcode) pair, calls `runLLC` to assemble to object file
3. Writes the runtime C library (embedded via Template Haskell `embedFile`)
4. Copies any additional C files into the temp directory
5. Runs `gcc` to compile and link all object files, the runtime, and C files
6. Copies the resulting executable to the output path

### `runLLC`

Writes bitcode to a `.bc` file, then runs `llc -filetype=obj -relocation-model=pic`
to produce an object file.

### `runGCC`

Runs `gcc` with:
- Debug flags: `-g`
- AddressSanitizer: `-fsanitize=address`
- Libraries: `-lgc` (Boehm GC), `-lgmp` (GMP)
- If `cc --version` indicates clang, also uses its flags; otherwise adds `-no-pie`
- Links `runtime.c`, C files, and object files into a `dist` executable

### `runtimeLib`

The runtime C library is embedded at compile time via `$(embedFile
"runtime/dist/runtime-combined.c")`. This combined C file is pre-built from the
runtime source using the `runtime/scripts/combine.sh` script.

---

## Compiler Interactions

- **Earlier passes this relies on**: KernelCodegen
- **Later passes**: None (this is the final pass)

---

## Important Data Structures

- `ByteString` — LLVM bitcode for each module
- `CompilerConfig` — controls output executable name (`configExecutableName`),
  additional C files (`configCFiles`), and verbosity (`configSilent`)

---

## Side Effects

- **Performs IO**: Runs `llc` and `gcc` subprocesses, creates temp files, writes executable
- **Generates diagnostics**: Prints executable path on success; prints errors on failure
- **Modifies compiler state**: No

---

## Notes

AddressSanitizer (`-fsanitize=address`) is always enabled in the default build
configuration. This helps catch memory errors during development but may need
to be disabled for production builds.