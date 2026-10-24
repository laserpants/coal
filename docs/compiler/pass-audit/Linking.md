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
`configCFiles`, and additional native libraries via `configNativeLibraries`
(rendered as `-l<name>` before the standard runtime libraries). Both are also
populated from every package in `coal.lock.json` — including transitive
dependencies — so a library can declare the native artifacts it needs without
the consuming project having to repeat them.

---

## Input

- `[(Name, ByteString)]` — list of (module name, LLVM bitcode) pairs

## Output

- `()` — side effect of producing the executable file

---

## Detailed Behavior

### `compileBitcode`

1. Creates a temporary working directory under the build cache (`.build/tmp`) and
   removes it afterwards. Staging intermediates there rather than in the system
   temporary directory matters on systems where `/tmp` is a RAM-backed tmpfs, which
   would charge every `.bc`/`.o` file (and the linked binary) to memory.
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
- AddressSanitizer: `-fsanitize=address`, only when `--sanitize` (`configSanitize`) is set
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
  additional C files (`configCFiles`), native libraries to link
  (`configNativeLibraries`, rendered as `-l<name>`), and verbosity
  (`configSilent`)

---

## Side Effects

- **Performs IO**: Runs `llc` and `gcc` subprocesses, creates temp files, writes executable
- **Generates diagnostics**: Prints executable path on success; prints errors on failure
- **Modifies compiler state**: Yes — drops all accumulated state except the
  active config and the source texts (needed by `compileWithCFiles` to render
  errors on failure) before spawning `llc`/`gcc`, so the live heap is
  trimmed to roughly the bitcode list when peak memory matters most

---

## Notes

AddressSanitizer (`-fsanitize=address`) is **opt-in**: it is added only when
`configSanitize` is set, i.e. when the compiler is invoked with `--sanitize` (or a
manifest sets `build.sanitize`). It is useful for catching memory errors during
development but slows compilation and linking noticeably, so it is off by default.