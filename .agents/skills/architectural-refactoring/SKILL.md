---
name: architectural-refactoring
description: Use when the task involves improving architecture, simplifying abstractions, reorganizing compiler stages, reducing technical debt, designing APIs, or making higher-level design decisions rather than implementing isolated local changes.
---

# Architectural Refactoring

## Objective

Improve the overall architecture of the compiler.

Focus on long-term maintainability, simplicity, and correctness.

Do not begin by editing code.

---

## Guiding Principles

Think at the level of:

- compiler stages
- abstractions
- data flow
- responsibilities
- ownership
- module boundaries
- APIs
- invariants

Avoid focusing on isolated implementation details until the larger design has been understood.

---

## Investigation Process

### 1. Understand the Current Design

Identify:

- responsibilities of each module
- data flow
- dependencies
- ownership of concepts
- architectural boundaries

Summarize the existing design before proposing changes.

---

### 2. Identify Pain Points

Look for:

- duplicated concepts
- leaky abstractions
- circular dependencies
- responsibilities split across modules
- excessive coupling
- unnecessary complexity
- violations of separation of concerns
- confusing APIs
- temporary migration code that should eventually disappear

Explain why each issue matters.

---

### 3. Consider Multiple Designs

Before recommending changes:

Generate multiple possible approaches.

For each approach discuss:

- advantages
- disadvantages
- implementation cost
- migration difficulty
- long-term maintainability

Do not immediately recommend the first idea.

---

### 4. Respect Existing Architecture

The compiler currently contains:

- legacy pipeline
- new pipeline
- LLVM backend
- runtime

During migration:

- preserve behavioural compatibility
- avoid unnecessary disruption
- avoid rewriting stable components
- prefer incremental migration

---

### 5. Recommend the Smallest Architectural Improvement

Prefer improvements that:

- simplify future work
- remove duplication
- clarify responsibilities
- reduce coupling
- preserve behaviour

Avoid "big bang" rewrites.

---

## Think in Terms of Compiler Design

Reason about:

- compiler phases
- intermediate representations
- ownership of transformations
- invariants between stages
- extensibility for future language features

Do not optimize individual functions until architectural questions have been answered.

---

## Output

Produce a report containing:

### Current Architecture

### Identified Problems

### Candidate Designs

### Recommended Direction

### Migration Strategy

### Risks

### Next Small Refactoring
