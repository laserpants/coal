# PhaseTypeChecking

## Purpose

Annotate the AST with kind and type information, expand aliases, prepare the build
environment, expand folds and function groups, and run type inference. Transforms
`Module Metadata () ()` to `Module Metadata Kind IndexedType`.

## Passes Executed

1. **KindIndexing** — assign kind annotations, setup name store, prepare build aliases
2. **Generate Debug Artifacts** — write kind-indexed IR for debugging
3. **ExpandFunctionGroups** — expand multi-equation function groups into let+match
4. **Generate Debug Artifacts** — write post-expansion IR
5. **ExpandAliases** — inline all type alias references
6. **Generate Debug Artifacts** — write post-alias-expansion IR
7. **PrepareBuild** — populate the build environment (types, constructors, traits, instances, imports)
8. **Generate Debug Artifacts + Build Info** — write post-build-prep IR
9. **ExpandTopLevelFolds** — expand top-level fold definitions into let expressions
10. **Generate Debug Artifacts** — write post-fold-expansion IR
11. **ExpandExpressionFolds** — expand inline fold expressions
12. **Generate Debug Artifacts** — write post-expr-fold IR
13. **ExpandLambdaMatchExpressions** — desugar lambda-match into lambda+match
14. **Generate Debug Artifacts** — write post-lambda-match IR
15. **TypeInference** — generate and solve kind and type constraints
16. **Generate Debug Artifacts + Build Info** — write post-inference IR
17. **ReportTypeErrors** — collect and report type errors
18. **Generate Debug Artifacts** — write final type-checked IR

## Execution Order

```
KindIndexing
  >-> generateDebugArtifacts "KindIndexing"
  >-> ExpandFunctionGroups
  >-> generateDebugArtifacts "ExpandFunctionGroups"
  >-> ExpandAliases
  >-> generateDebugArtifacts "ExpandAliases"
  >-> PrepareBuild
  >-> generateDebugArtifacts "PrepareBuild"
  >-> generateBuildInfo "PrepareBuild"
  >-> ExpandTopLevelFolds
  >-> generateDebugArtifacts "ExpandTopLevelFolds"
  >-> ExpandExpressionFolds
  >-> generateDebugArtifacts "ExpandExpressionFolds"
  >-> ExpandLambdaMatchExpressions
  >-> generateDebugArtifacts "ExpandLambdaMatchExpressions"
  >-> TypeInference
  >-> generateDebugArtifacts "TypeInference"
  >-> generateBuildInfo "TypeInference"
  >-> ReportTypeErrors
  >-> generateDebugArtifacts "ReportTypeErrors"
```

## Inputs

- `Module Metadata () ()` — per-module, untyped, no kind info

## Outputs

- `Module Metadata Kind IndexedType` — per-module, fully kind-annotated and
  type-inferred

## Invariants Established by the Phase

- Every type parameter has a `Kind` annotation
- Every expression and pattern has an `IndexedType` annotation
- Type aliases are fully expanded
- Build environment is populated with type constructors, data constructors,
  traits, instances, imports, and qualified name mappings
- Top-level and expression folds are expanded into let bindings
- Function groups are expanded into individual let definitions with pattern matching
- Lambda-match expressions are desugared into lambda + match
- Kind and type constraints are generated and solved
- Type errors are reported as diagnostics

## Invariants Expected by Later Phases

The translation phase expects:
- Fully type-annotated AST (kind and indexed type)
- No type aliases remaining (all expanded)
- No fold expressions remaining (all expanded)
- No function groups (all expanded to individual lets)
- Valid type environment with no errors