# ffi-float-return — float/double foreign-call ABI regression fixture

Guards that a foreign (FFI) function whose declared return type is `float` or
`double` delivers its result through the register the code generator reads.

The LLVM code generator declares a foreign function with its **raw** return
type and reads the result out of the corresponding machine register
(`Coal.Kernel.LLVM.Codegen.ECall` / `irTailECall`): arguments are boxed to
`ptr`, but the result is *not* boxed by the callee — the generator boxes it
itself. See `test/regression/ffi-in-return/counter.c` for the integer case.

The stdlib wrappers in `runtime/src/value_api.c` used to violate this: they
returned a boxed `rt_value_t` (a heap pointer) for `float`/`double` results.
Because `int32`/`int64`/`bool`/`char` boxes are identity casts, integer
results were unaffected; `float`/`double` results only appeared correct
because the value happened to remain in `xmm0` (register leftover). That is
not guaranteed by the ABI — e.g. a differently optimized runtime, or any
future change to `rt_double_box`, would expose it.

> **Not part of `stack test`.** Run it manually:

    COAL=/path/to/coal bash run.sh

`PASS: ffi-float-return returns float/double values correctly on …` means
every conversion printed the expected value.

## What it covers

* `Number.int32_to_float` / `int32_to_double`
* `Number.int64_to_float` / `int64_to_double`
* `Number.float_to_double` / `double_to_float`
* a user foreign function returning an unboxed `double` (`ffi_float.c`), which
  clobbers `xmm0` before returning

## Artifacts

Running leaves `.build/`, `coal.lock.json`, and the produced
`ffi-float-return-*` binary here; all are covered by the repository
`.gitignore`. `run.sh` deletes the binary again on success and keeps
everything for inspection on failure.
