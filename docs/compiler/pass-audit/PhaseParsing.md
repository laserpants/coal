# PhaseParsing

## Purpose

Parse source files from disk into untyped AST modules. This is the first phase
of the compiler pipeline. It reads Coal source files, runs the Megaparsec-based
parser, and resolves module paths. It also parses embedded builtin modules.

## Passes Executed

A single pass runs in this phase:

1. **Parsing** (`passParsing`)

## Execution Order

```
Parsing
```

## Inputs

- `[FilePath]` — list of source file paths to compile

## Outputs

- `[BuildEnvelope (Module Metadata () ())]` — list of build envelopes wrapping
  untyped AST modules (no kind annotations, no type annotations). Each envelope
  is either `BSource` (freshly parsed) or `BCached` (retrieved from build cache).

## Invariants Established by the Phase

- Each file path resolves to a valid source file
- The module name declared in the source matches the file path
- Syntax is valid Coal (no parse errors)
- Builtin modules are embedded and available
- Modules are registered in the compiler's source store for later phases

## Invariants Expected by Later Phases

Later phases expect:
- Every `BuildEnvelope` contains a syntactically valid `Module Metadata () ()`
- Module names match their file paths
- All imports refer to modules that exist in the compilation unit (this is
  verified by `SortModules` in the preflight phase)
- No cache invalidation checking has been done yet (that happens in `RefreshCache`)

## Notes

The parsing phase also handles caching: if a module's source hasn't changed
since the last build, the cached build is returned as `BCached`. The actual
cache freshness checking happens later in `RefreshCache`.