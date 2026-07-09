# Parsing

## Purpose

Reads Coal source files from disk, parses them into the untyped AST (`Module Metadata () ()`),
and handles build caching. This is the only pass in the `PhaseParsing` phase.

---

## Location

```text
src/Coal/Compiler/Pass/PhaseParsing/Parsing.hs
```

---

## Summary

Parses Coal source text into the compiler's AST representation. Deals with
three categories of input: user source files, embedded builtin modules, and
previously cached builds. For each module, either returns a freshly parsed
`BSource` envelope or a `BCached` envelope if the source hasn't changed
since the last build.

---

## Input

- **AST representation**: Raw `FilePath` strings
- **Assumptions**: Files exist and are readable
- **Required invariants**: None (this is the first pass)

---

## Output

- **Resulting AST**: `[BuildEnvelope (Module Metadata () ())]` — modules with
  no kind or type annotations
- **Established invariants**:
  - All modules parsed successfully (no parser errors)
  - Module names match their file paths
  - Source text registered in `compilerSources`
- **Guarantees made to later passes**: Syntactically valid Coal AST

---

## Detailed Behavior

### `passImpl`

The main entry point. It:
1. Parses all embedded builtin modules from `builtinModules`
2. Reports errors for any builtin parse failures
3. Parses all user source files via `parseFile`
4. Reports parse errors per file
5. Returns the concatenation of builtin and user module bundles

### `parseFile`

For each file path:
1. Resolves the file via `resolveModule` (searches source paths for the file)
2. Reads the file contents as UTF-8 text
3. Calls `fromSource` to parse and check the filename/name correspondence

### `fromSource`

1. Runs `parseSourceFile` (Megaparsec parser) on the source text
2. Checks that the module's declared name matches the file path
3. Calls `checkCacheAndRegister` to handle caching

### `checkCacheAndRegister`

1. Registers the source text in `compilerSources`
2. Checks for a valid cached build via `cachedBuild`
3. If a valid cache exists and caching is not disabled (`configNoCache`),
   returns `BCached`
4. Otherwise, marks the module as "touched" and returns `BSource`

### `parseEmbedded`

Used for builtin modules stored as embedded `ByteString`s in the compiler binary.
Performs the same parse-check-cache flow but reports parse failures differently
(they are considered compiler bugs since builtins should always parse).

---

## Transformation Rules

No AST transformations are performed. The parser converts source text directly
to the AST as defined by the grammar.

---

## Analysis

- **Tree traversals**: None (the parser builds the AST directly)
- **I/O**: Reads files from disk, checks build cache
- **Error handling**: Parse errors are reported as `ParserError` and cause
  `ParserFailure`

---

## Compiler Interactions

- **Earlier passes this relies on**: None (first pass)
- **Later passes that rely on this pass**: All subsequent passes depend on
  valid parsed AST

---

## Important Data Structures

- `BuildEnvelope` — wraps modules with caching information (`BSource` vs `BCached`)
- `Build` — cached build information including dependencies and hashes
- `CompilerConfig` — controls caching behavior via `configNoCache`

---

## Side Effects

- **Generates diagnostics**: Parse errors, bad module name errors, bad filename errors
- **Modifies compiler state**: Registers source text in `compilerSources`,
  inserts cached builds, sets touched modules
- **Caches information**: Checks build cache via `cachedBuild`
- **Performs IO**: File reading, path resolution

---

## Notes

The parsing pass uses Megaparsec for parsing. The parser itself lives in
`src/Coal/Parser/`. Embedded builtin modules are stored as `ByteString`s
compiled into the binary via `src/Coal/Compiler/Builtin/Modules.hs`.