---
name: trace-compiler-transformation
description: Use when tracing a source construct, expression, type, value, generated LLVM instruction, or runtime behaviour through multiple compiler stages to identify where semantics diverge. Particularly useful when investigating regressions between the legacy and new compiler pipelines.
---

# Trace Compiler Transformation

## Objective

Determine where a semantic change is introduced during compilation.

The goal is **not** to fix the bug immediately.

The goal is to identify the **first compiler stage where the behaviour diverges from the expected semantics**.

---

## Guiding Principles

- Trace one thing at a time.
- Follow the data, not the files.
- Gather evidence before making edits.
- Prefer understanding over speculation.
- Stop when the first incorrect transformation has been identified.
- The legacy compiler is the behavioural reference unless instructed otherwise.

---

## When To Use

Use this skill when:

- a compiled program crashes
- generated LLVM IR appears incorrect
- the new compiler behaves differently from the legacy compiler
- a value appears to change unexpectedly during compilation
- a generated instruction appears suspicious
- a type or expression is transformed incorrectly
- the source of a regression is unknown

---

## Inputs

Identify one or more of the following:

- source program
- source expression
- AST node
- type
- generated LLVM instruction
- runtime value
- failing function
- compiler stage
- test case

If none of these are available, determine the smallest observable symptom before continuing.

---

## Investigation Procedure

### Step 1 — Define the Subject

Clearly identify what is being traced.

Examples:

- one expression
- one variable
- one function
- one generated LLVM instruction
- one runtime value

Avoid tracing multiple unrelated things simultaneously.

---

### Step 2 — Locate the Origin

Determine where the subject first appears.

Possible locations include:

- parser
- typed AST
- kernel language
- intermediate representations
- LLVM generation
- runtime

---

### Step 3 — Trace Stage by Stage

Follow the subject through every compiler stage.

For each stage:

- identify the corresponding representation
- explain how it changed
- verify that semantics were preserved
- note any assumptions

Do not skip stages without explanation.

---

### Step 4 — Compare With the Legacy Pipeline

If the legacy compiler can produce the same program:

- compare representations
- ignore formatting
- ignore generated identifiers
- ignore cosmetic differences

Focus only on semantic differences.

---

### Step 5 — Identify the First Divergence

Locate the earliest point where:

- behaviour changes
- information is lost
- an incorrect transformation occurs
- an invariant is violated

This is usually far more useful than analysing the final LLVM IR in isolation.

---

### Step 6 — Explain the Root Cause

Summarize:

- what changed
- where it changed
- why it changed
- why the change is incorrect

Support conclusions with evidence.

Avoid speculation.

---

### Step 7 — Recommend a Fix

Only after the divergence has been identified:

- identify the smallest code change likely to correct the issue
- explain why it should work
- identify any risks
- mention additional tests that should be added

---

## Investigation Strategy

Prefer this order:

1. Evidence
2. Observations
3. Hypotheses
4. Verification
5. Code changes

Avoid jumping directly to implementation.

---

## Things To Avoid

Do not:

- rewrite unrelated code
- perform opportunistic refactoring
- optimise while debugging correctness
- compare entire compiler outputs when one value can be traced instead
- propose fixes without identifying the first semantic divergence

---

## Expected Output

Produce a concise report containing:

### Subject

What was traced.

### Compiler Stages

How it changed through each stage.

### First Divergence

The first stage where semantics changed.

### Evidence

The observations supporting the conclusion.

### Recommended Fix

The smallest reasonable correction.

### Confidence

State whether the conclusion is:

- High
- Medium
- Low

If confidence is not high, explain what additional evidence would increase confidence.
