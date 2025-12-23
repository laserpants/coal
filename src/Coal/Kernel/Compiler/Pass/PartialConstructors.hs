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
                  (NonEmpty.append es (Syntax.var <$> NonEmpty.fromList qs))
              )
        | otherwise ->
            Syntax.app u (Syntax.var ll) es
       where
        ps = labels t
        qs = NonEmpty.drop (length es) ps
      Syntax.EVar ll@(Label t var)
        | isConstructor var && arity t > 0 -> do
            Syntax.lam ps (Syntax.app Syntax.opaque (Syntax.var ll) (Syntax.var <$> ps))
        | otherwise ->
            Syntax.var ll
       where
        ps = labels t
      e ->
        embed e

labels :: Type -> NonEmpty (Label Type)
labels t = uncurry Label <$> NonEmpty.zip (unfoldType t) ls
 where
  ls = (\t0 -> "$#arg" <> showt t0) <$> (1 :| [2 .. arity t])
