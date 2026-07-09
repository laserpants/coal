# DebugOutput

## Purpose

Utility pass for generating debug artifacts and build information during compilation.
Not a standalone compiler pass but used as an interleaved debugging step between
other passes.

---

## Location

```text
src/Coal/Compiler/Pass/DebugOutput.hs
```

---

## Summary

Provides two pass-like functions:

- `generateDebugArtifacts :: (MonadIO m) => String -> Pass a m o o` —
  when `configGenerateDebugArtifacts` is enabled, dumps the current module's
  pretty-printed IR to a debug file named after the stage label.

- `generateBuildInfo :: (MonadIO m) => String -> Pass a m o o` —
  when `configGenerateDebugArtifacts` is enabled, dumps additional build
  information to a debug file.

Both are identity transforms in terms of output (they return the input unchanged).

---

## Usage

Inserted between passes in the pipeline:

```
passKindIndexing
  >-> generateDebugArtifacts "KindIndexing"
  >-> passExpandFunctionGroups
  >-> generateDebugArtifacts "ExpandFunctionGroups"
  ...
```

Each call writes a file like `.debug/KindIndexing/<ModuleName>.coal` containing
the pretty-printed module IR at that stage.

---

## Side Effects

- **Performs IO**: Writes debug files to disk when `configGenerateDebugArtifacts` is enabled
- **Generates diagnostics**: No
- **Modifies compiler state**: No