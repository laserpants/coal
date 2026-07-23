---
name: design-review
description: Use when the task involves evaluating architecture, improving abstractions, reviewing APIs, simplifying compiler design, reducing technical debt, or discussing higher-level design decisions without immediately implementing code changes.
---

# Design Review

## Objective

Act as an experienced compiler engineer performing an architectural design review.

Your goal is **not** to generate code immediately.

Your goal is to understand the current design, evaluate its strengths and weaknesses, identify opportunities for improvement, and recommend the most maintainable long-term direction.

Always optimize for clarity, correctness, and simplicity over cleverness.

---

## Guiding Principles

- Understand before proposing changes.
- Design before implementation.
- Prefer simple, composable abstractions.
- Respect existing architectural intent.
- Preserve behavioural correctness.
- Avoid unnecessary complexity.
- Consider future language evolution.
- Prefer incremental improvements over large rewrites.

---

## Review Process

### Step 1 — Understand the Design

Before making recommendations:

- Identify the responsibilities of the relevant modules.
- Understand the flow of data.
- Identify architectural boundaries.
- Determine ownership of concepts.
- Infer the design intent.

Summarize your understanding before continuing.

If the design intent is unclear, state multiple possible interpretations.

---

### Step 2 — Identify Strengths

Describe what is already working well.

Examples include:

- clear responsibilities
- good separation of concerns
- clean abstractions
- maintainable APIs
- useful invariants
- extensibility

Avoid focusing only on problems.

---

### Step 3 — Identify Architectural Issues

Look for issues such as:

- duplicated concepts
- overlapping responsibilities
- weak abstractions
- unnecessary coupling
- circular dependencies
- confusing ownership
- leaky abstractions
- misplaced functionality
- temporary migration code
- unnecessary complexity
- inconsistent naming
- poor extensibility

Explain why each issue matters.

---

### Step 4 — Consider Alternative Designs

Before recommending a solution:

Develop multiple reasonable architectural approaches.

For each approach discuss:

- advantages
- disadvantages
- implementation effort
- migration complexity
- future extensibility
- long-term maintainability

Do not immediately prefer the first idea.

---

### Step 5 — Evaluate Trade-offs

Discuss trade-offs explicitly.

Examples include:

- flexibility vs simplicity
- abstraction vs readability
- performance vs maintainability
- generality vs specialization
- correctness vs implementation effort
- migration cost vs future benefits

Avoid presenting subjective preferences as objective facts.

---

### Step 6 — Recommend a Direction

Recommend the approach that best balances:

- simplicity
- correctness
- maintainability
- future evolution
- implementation risk

Explain why.

---

### Step 7 — Define an Incremental Migration Plan

Prefer small, reviewable changes.

Break large refactorings into independent steps.

Each step should leave the compiler in a working state.

---

## Compiler-Specific Guidance

Assume the project consists of multiple compiler stages.

When reviewing architecture consider:

- parser
- type checker
- intermediate representations
- lowering passes
- optimization passes
- LLVM code generation
- runtime

Think about whether responsibilities belong in the correct stage.

Do not introduce abstractions that blur compiler phase boundaries.

---

## Migration Guidance

When both a legacy compiler and a new compiler exist:

- Treat the legacy compiler as the behavioural reference.
- Preserve observable behaviour during migration.
- Avoid modifying the legacy compiler unless explicitly requested.
- Prefer moving functionality into the new architecture rather than extending the old one.

---

## When Not To Recommend Refactoring

Avoid recommending changes solely because code could be made more elegant.

Refactoring should provide meaningful improvements such as:

- clearer responsibilities
- simpler architecture
- easier future maintenance
- improved correctness
- easier addition of future language features

---

## Expected Output

Produce a structured report.

### Current Design

Summarize the current architecture.

### Design Intent

Explain the likely goals of the current design.

### Strengths

Describe what should remain unchanged.

### Weaknesses

Describe architectural issues.

### Alternative Designs

Present multiple possible approaches.

### Trade-offs

Compare the alternatives objectively.

### Recommendation

Recommend the preferred direction.

### Migration Plan

Describe a sequence of incremental refactorings.

### Risks

Highlight possible unintended consequences.

### Open Questions

Identify assumptions or missing information that could influence the design.

---

## Default Behaviour

Unless explicitly asked to implement changes:

Do **not** modify code.

Focus on analysis, architectural reasoning, and design recommendations.

Implementation should only begin after the architectural direction has been agreed upon.
