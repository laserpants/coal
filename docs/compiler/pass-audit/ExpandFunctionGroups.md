# ExpandFunctionGroups

## Purpose

Expand function groups (multiple equations defining a single function) into individual
let definitions with lambda expressions and explicit pattern matching.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTypeChecking/ExpandFunctionGroups.hs
```

---

## Summary

Converts `DFunctionGroup` definitions into `DLet` definitions. Each branch of the
function group becomes a clause in a match expression, and the function's parameters
are packed into a tuple (or kept as a single variable for unary functions).

---

## Input

- **AST representation**: `Module Metadata Kind ()`
- **Required invariants**: Kind indexing completed

---

## Output

- **Resulting AST**: `Module Metadata Kind ()` — no `DFunctionGroup` definitions
  remain
- **Established invariants**: All multi-equation functions are represented as
  let+lambda+match

---

## Detailed Behavior

### `expandGroups`

For `DFunctionGroup`:
1. Generates argument names (`$arg_1`, `$arg_2`, ...) based on the first branch's
   pattern count
2. Creates a let definition whose body is `fn($arg_1, ...) => match(tuple_of_args) { branches }`
3. For unary functions, the argument is kept as a single variable; for multi-arg,
   arguments are packed into a tuple pattern

### `buildExpressionClauses`

Converts each `FunctionDefinition` branch into a `Clause` with its pattern and
expression body.

### `packVariables` / `packPatterns`

For unary functions, returns the single variable/pattern directly. For multi-arg
functions, wraps them in a tuple.

---

## Transformation Rules

```
fun f
  | 0 = "zero"
  | n = "non-zero"
```
becomes:
```
let f = fn($arg_1) => match($arg_1) {
  | 0 => "zero"
  | n => "non-zero"
}
```

---

## Compiler Interactions

- **Earlier passes this relies on**: KindIndexing
- **Later passes that rely on this pass**: ExpandAliases (must run after groups
  are expanded)

---

## Side Effects

- **Generates diagnostics**: No
- **Modifies compiler state**: No
- **Creates fresh names**: Argument names `$arg_N`