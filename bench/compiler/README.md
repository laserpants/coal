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
- **Coverage is normalization-only.** There are no benchmarks for parsing,
  type checking, translation, lowering, LLVM code generation, or the
  end-to-end `coal compile` flow.
- **Inputs are loaded with `unsafePerformIO`** and crash the benchmark at
  startup if a `.corn` file is missing or fails to parse.
