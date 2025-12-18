{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Compiler.Pass.PartialConstructors (saturateConstructors) where

import Coal.Common.Label (Label (..))
import Coal.Kernel.Language (Expr, Type)
import qualified Coal.Kernel.Language as Syntax
import Coal.Kernel.Language.Type.Arrow (arity, unfoldType)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata, embed)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Extras (isConstructor)
import TextShow (showt)

saturateConstructors :: Expr Type -> Expr Type
saturateConstructors =
  cata $
    \case
      Syntax.EApp u (Fix (Syntax.EVar ll@(Label t var))) es
        | isConstructor var && arity t > length es ->
            Syntax.lam
              ps
              ( Syntax.app
                  Syntax.opaque
                  (Syntax.var ll)
                  (NonEmpty.append es (Syntax.var <$> ps))
              )
        | otherwise ->
            Syntax.app u (Syntax.var ll) es
       where
        n = arity t
        ts = NonEmpty.drop (length es) (unfoldType t)
        ls = (\t0 -> "a" <> showt t0) <$> [1 .. n - length es]
        ps = NonEmpty.fromList (uncurry Label <$> zip ts ls)
      Syntax.EVar ll@(Label t var)
        | isConstructor var && arity t > 0 -> do
            Syntax.lam ps (Syntax.app Syntax.opaque (Syntax.var ll) (Syntax.var <$> ps))
        | otherwise ->
            Syntax.var ll
       where
        n = arity t
        ts = unfoldType t
        ls = (\t0 -> "a" <> showt t0) <$> (1 :| [2 .. n])
        ps = uncurry Label <$> NonEmpty.zip ts ls
      e ->
        embed e
