# ExpandLetBindings

## Purpose

Rewrite multi-binding `let` expressions into nested single-binding `let`
expressions, giving the bindings sequential (left-to-right) scope.

---

## Location

```text
src/Coal/Compiler/Pass/PhasePreflight/ExpandLetBindings.hs
```

---

## Summary

The surface syntax

```
let a = e1;
    b = e2
  in e3
```

is parsed into a single `ELet` node with two bindings. This pass rewrites it
into `let a = e1 in let b = e2 in e3`, so each binding's right-hand side sees
all bindings to its left. Single-binding lets and `ERecursiveLet` nodes are
left untouched.

---

## Input

- **AST representation**: `[BuildEnvelope (Module Metadata () ())]`
- **Required invariants**: Builtins inserted, where-clauses and do-notation
  desugared

---

## Output

- **Resulting AST**: Same type, with no multi-binding `ELet` remaining at the
  surface-language level
- **Established invariants**: `let` bindings have sequential scope
- **Guarantees made to later passes**: Surface-language `ELet` nodes bind
  exactly one name group; a binding's RHS may refer to earlier bindings of the
  same source-level group

---

## Detailed Behavior

### `expandLetBindings`

For an `ELet` with more than one binding, folds the bindings from the right:
each binding becomes a single-binding `ELet` wrapping the rest. The outermost
node of the resulting chain keeps the metadata of the original group; each
nested node takes the metadata of its binding. The rewrite is recursive
(bottom-up via uniplate `transform`), so let-groups nested inside binding
right-hand sides and bodies are expanded as well.

### `expandLetBindingsModule`

Applies `expandLetBindings` over the whole module (uniplate `transformBi`).

---

## Analysis

- **Tree traversals**: Bottom-up via uniplate `transform` / `transformBi`
- **Fresh-name generation**: No
- **Recursion**: Single pass; no fresh names are introduced

---

## Compiler Interactions

- **Earlier passes this relies on**: DesugarDoNotation
- **Later passes that rely on this pass**: DetectShadowing, DetectDuplicateParams
  (duplicate names within a source-level group now surface as shadowing
  errors), and the constraint generator `emitELetConstraints`, which types a
  single-binding `ELet` sequentially: the body's assumptions (including later
  group members' right-hand sides) are asserted against the binding pattern.

---

## Side Effects

- **Generates diagnostics**: No
- **Modifies compiler state**: No

---

## Notes

This is a semantics change relative to earlier versions of the language,
where a multi-binding group was treated as simultaneous (equivalent to
`let (a, b) = (e1, e2) in e3`). Programs that relied on a group binding
capturing an outer-scope variable of the same name will now fail with a
shadowing error instead of silently capturing.

The kernel-level multi-binding `ELet` (produced internally, e.g. by `EFocus`
desugaring) is not affected: this pass runs on the surface AST only.
