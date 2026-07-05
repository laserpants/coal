---
name: compare-pipelines
description: Use when comparing the legacy compiler pipeline and the new compiler pipeline to locate regressions or behavioural differences.
---

# Compare Compiler Pipelines

## Goal

Locate the first semantic difference between the old and new compiler.

## Method

Compare compiler stages in order.

1. Parser
2. Typed AST
3. Kernel language
4. LLVM IR
5. Runtime behaviour

Do not compare everything simultaneously.

At each stage:

- Ignore formatting.
- Ignore generated names.
- Focus only on semantic differences.

Stop once the first behavioural divergence is found.

Do not propose fixes until the divergence has been identified.
