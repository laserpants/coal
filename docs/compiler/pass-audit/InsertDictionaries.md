# InsertDictionaries

## Purpose

Transform trait constraints into explicit dictionary-passing style. Inserts trait
dictionaries as extra parameters at call sites and wraps constrained definitions in
dictionary lambdas.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseTranslation/InsertDictionaries.hs
```

---

## Summary

Implements dictionary-passing for type classes (traits). Functions with trait
constraints are transformed to accept explicit dictionary parameters (records
containing trait methods). At call sites, the compiler looks up appropriate
trait instances and inserts dictionary arguments. The pass runs twice to handle
all trait dependencies correctly.

---

## Input

- **AST representation**: `Module Metadata Kind IndexedType`
- **Required invariants**: Match expressions compiled

---

## Output

- Same type with trait dictionaries inserted at all call sites and all
  constrained definitions wrapped in dictionary lambdas

---

## Detailed Behavior

### Main pass flow

1. Extracts the type environment from the build
2. Runs `collectDefinitionTraits` twice (first pass collects trait info,
   second pass resolves dependencies)
3. Updates name store with fresh schemes
4. Runs `insertTraitDictionaries` on each definition

### `expandTraits` (Expression instance)

For `EVariable` nodes:
1. Calls `collectTraits` to get the trait constraints for the name at the
   inferred type
2. Calls `applyTraits` which looks up instances and either inserts a
   concrete dictionary or emits a trait instance variable

For `ELet` bindings:
1. Runs `transformBindingWithTraits` on each binding
2. If traits are discovered, wraps the binding body in a `dictionaryLambda`
   that accepts trait dictionaries as extra parameters

### `lookupTraitInstance`

Uses `findFirstMatch` to search `buildInstances` for a matching instance.
For concrete types, reports `MissingInstance` if none found. For type
variables, returns `Nothing` (the constraint is passed through).

### `dictionaryLambda`

Creates `fn(impl_Show, impl_Eq) => body` for each required trait, using
`PTraitInstance` patterns to bind the dictionary parameters.

### `expandLetDefinitionTraits`

Special-cases `main` function: if it has trait constraints on a type
variable, inserts a default `int32` instance. For other definitions, wraps
in dictionary lambdas that accept the trait dictionaries.

### Two-pass strategy

1. **Passive pass** (`passiveExpandTraitsInExpr`): collects trait constraints
   without modifying the expression body, registers names with inferred types
2. **Active pass** (`expandTraits`): transforms expressions to insert actual
   dictionary parameters and arguments

---

## Transformation Rules

```
fun show(x: a) with Show<a> = ...
show(42 : int32)
```
becomes:
```
let show = fn(impl_Show : Show<a>, x : a) => ...
show({Show<int32>.show = ...}, 42 : int32)
```

---

## Analysis

- **Instance resolution**: Matches trait constraints against registered instances
  via `tryMatch` (type unification)
- **Supply monad**: Used for creating fresh type variables during unification
- **Journal**: Uses `listenDictionaryTraits` / `tellDictionaryTraits` /
  `censorDictionaryTraits` to collect and propagate trait constraints

---

## Compiler Interactions

- **Earlier passes this relies on**: CompileMatchExpressions, PrepareBuild (instances)
- **Later passes that rely on this pass**: CompileNats

---

## Side Effects

- **Generates diagnostics**: `MissingInstance`, `TraitNotInScope` for missing trait implementations
- **Modifies compiler state**: Updates name store with inferred schemes
- **Creates fresh names**: Instance labels, fresh type variables for unification