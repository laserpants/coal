{-# LANGUAGE LambdaCase #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.ExpandLetBindings

Rewrite multi-binding @let@ expressions into nested single-binding @let@
expressions, giving the bindings sequential (left-to-right) scope.

The surface syntax

@
let a = e1;
    b = e2
  in e3
@

is parsed into a single @ELet@ node holding both bindings. Previously the
bindings were typed and translated as a simultaneous group: no binding's
right-hand side could refer to any binding of the same group. This pass
rewrites the group into

@
let a = e1 in
  let b = e2 in
    e3
@

so that each binding's right-hand side sees all bindings to its left. This is
a conservative extension: programs that were previously well-typed (and did
not rely on an outer-scope variable being captured by a same-named group
binding) keep their meaning.

Single-binding @let@ expressions and recursive lets ('ERecursiveLet') are left
untouched. The kernel-level multi-binding @ELet@ (used internally, e.g. by
record focus desugaring) is not affected by this pass.
-}
module Coal.Compiler.Pass.PhasePreflight.ExpandLetBindings (
  passExpandLetBindings,
  expandLetBindingsModule,
  expandLetBindings,
) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass)
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Module (Module (..))
import Control.Monad.IO.Class (MonadIO)
import Data.Generics.Uniplate.Data (transform, transformBi)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

{- | Let-binding expansion pass.

Rewrite every multi-binding @let@ expression in the module into a chain of
nested single-binding @let@ expressions.
-}
passExpandLetBindings :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passExpandLetBindings = mapPass $ Pass{runPass = traverse passImpl}

passImpl :: (MonadIO m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata () ())
passImpl = pure . expandLetBindingsModule

{- | Apply 'expandLetBindings' to every expression in a module.

The traversal is bottom-up (uniplate 'transform'), so let-groups nested inside
binding right-hand sides are expanded before their enclosing group.
-}
expandLetBindingsModule :: Module Metadata () () -> Module Metadata () ()
expandLetBindingsModule m =
  transformBi
    ( expandLetBindings ::
        Expression Metadata () () -> Expression Metadata () ()
    )
    m

{- | Rewrite a multi-binding let expression into nested single-binding let
expressions, recursively. Single-binding lets and all other expressions are
returned unchanged.

The traversal is bottom-up (uniplate 'transform'), so let-groups nested inside
binding right-hand sides and bodies are expanded before their enclosing group.

The outermost node of a resulting chain keeps the metadata of the original
group; each nested node takes the metadata of its binding.
-}
expandLetBindings :: Expression Metadata () () -> Expression Metadata () ()
expandLetBindings =
  transform $ \case
    ELet loc bindings body
      | NonEmpty.length bindings > 1 ->
          nestBindings loc bindings body
    e ->
      e

nestBindings :: Metadata -> NonEmpty (Binding Expression Metadata () ()) -> Expression Metadata () () -> Expression Metadata () ()
nestBindings loc bindings = go True (NonEmpty.toList bindings)
 where
  go _ [] =
    id
  go isFirst (b : bs) =
    \body ->
      ELet
        (if isFirst then loc else bindingMetadata b)
        (b :| [])
        (go False bs body)

bindingMetadata :: Binding Expression Metadata () () -> Metadata
bindingMetadata (BPattern a _ _) = a
bindingMetadata (BFunction a _ _ _) = a
