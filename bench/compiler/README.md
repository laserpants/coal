# Compiler benchmarks

This benchmark suite measures the performance of the Coal compiler's **kernel
normalization passes**. Each pass is benchmarked in isolation, plus the full
`pipeline`, on parsed kernel IR (`.corn`) modules taken from `test/examples/`.

## Running

```sh
stack bench coal-bench-compiler                    # run all benchmarks
stack bench coal-bench-compiler --ba='--list'      # list benchmark names
stack bench coal-bench-compiler --ba='--match "anf"'  # filter by regex
stack bench coal-bench-compiler --ba='--output report.html'  # HTML report
```

## Known limitations

- **`NFData` is `show`-based.** `NFData (Module Type)` is implemented as
  `rnf m = rnf (show m)`, which adds serialization overhead that scales with
  output size. Cross-pass comparisons are therefore approximate, not exact.
- **Coverage is normalization-only.** Parsing, translation, lowering, LLVM code
  generation, and the end-to-end `coal compile` flow are not benchmarked. The
  `typechecking` group covers the type constraint solver and substitution
  application; constraint *generation* and the kind solver are not benchmarked.
- **Inputs are loaded with `unsafePerformIO`** and crash the benchmark at
  startup if a `.corn` file is missing or fails to parse.
