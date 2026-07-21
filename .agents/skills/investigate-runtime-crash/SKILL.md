---
name: investigate-runtime-crash
description: Use when the user is debugging a runtime crash, segmentation fault, invalid LLVM IR, undefined behaviour, or a regression between the legacy and new compiler pipeline.
---

# Investigate Runtime Crash

## Objective

Determine the root cause of a runtime failure while minimizing speculative code changes.

## Principles

- Gather evidence before editing code.
- Treat the legacy compiler as the behavioural specification.
- Prefer understanding over patching.
- Ignore cosmetic LLVM IR differences.

## Procedure

1. Determine exactly how the program fails.
2. Identify the faulting instruction or runtime location.
3. Read only the relevant generated LLVM IR.
4. Trace the instruction back through the code generator.
5. Compare with the equivalent output from the legacy compiler.
6. List concrete hypotheses.
7. Collect evidence to eliminate hypotheses.
8. Recommend the smallest change that explains the evidence.

## Do Not

- Rewrite unrelated code.
- Refactor while debugging.
- Guess at fixes without evidence.
- Introduce optimizations while investigating correctness.
