module Coal.AST.Shorthand (
  matchE,
  varE,
  letE,
  ifE,
  applicationE,
  lambdaE,
  lambda1E,
  lambdaAnyE,
  selectE,
  tupleE,
  literalBoolE,
  plainClauseE,
  opAndE,
  opEqualToE,
  varP,
  constructorP,
  anyP,
  tupleP,
  funDef,
) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module (FunctionDef (..))
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name)

{-# INLINE matchE #-}
matchE :: (Monoid a) => Expression a () -> NonEmpty (Clause a ()) -> Expression a ()
matchE = EMatch mempty ()

{-# INLINE varE #-}
varE :: (Monoid a) => Name -> Expression a ()
varE = EVariable mempty . label

{-# INLINE ifE #-}
ifE :: (Monoid a) => Expression a () -> Expression a () -> Expression a () -> Expression a ()
ifE = EIf mempty ()

{-# INLINE letE #-}
letE :: (Monoid a) => Name -> Expression a () -> Expression a () -> Expression a ()
letE name = ERecursiveLet mempty (PVariable mempty (label name))

{-# INLINE applicationE #-}
applicationE :: (Monoid a) => Expression a () -> NonEmpty (Expression a ()) -> Expression a ()
applicationE = EApplication mempty ()

{-# INLINE lambdaE #-}
lambdaE :: (Monoid a) => NonEmpty (Pattern a ()) -> Expression a () -> Expression a ()
lambdaE = ELambda mempty

{-# INLINE lambda1E #-}
lambda1E :: (Monoid a) => Name -> Expression a () -> Expression a ()
lambda1E var = ELambda mempty (PVariable mempty (label var) :| [])

{-# INLINE lambdaAnyE #-}
lambdaAnyE :: (Monoid a) => Expression a () -> Expression a ()
lambdaAnyE = ELambda mempty (PAny mempty () :| [])

{-# INLINE selectE #-}
selectE :: (Monoid a) => Name -> Expression a () -> Expression a ()
selectE = ESelect mempty . label

{-# INLINE tupleE #-}
tupleE :: (Monoid a) => NonEmpty (Expression a ()) -> Expression a ()
tupleE = ETuple mempty ()

{-# INLINE literalBoolE #-}
literalBoolE :: (Monoid a) => Bool -> Expression a ()
literalBoolE = ELiteral mempty . LBool

{-# INLINE opEqualToE #-}
opEqualToE :: (Monoid a) => Expression a ()
opEqualToE = EBinaryOperator mempty () OEqualTo

{-# INLINE opAndE #-}
opAndE :: (Monoid a) => Expression a ()
opAndE = EBinaryOperator mempty () OLogicalAnd

{-# INLINE plainClauseE #-}
plainClauseE :: (Monoid a) => Pattern a () -> Expression a () -> Clause a ()
plainClauseE p e = EClause mempty p (CPlain mempty [] e :| [])

{-# INLINE label #-}
label :: Name -> Label ()
label = Label ()

{-# INLINE varP #-}
varP :: (Monoid a) => Name -> Pattern a ()
varP = PVariable mempty . Label ()

{-# INLINE constructorP #-}
constructorP :: (Monoid a) => Name -> [Pattern a ()] -> Pattern a ()
constructorP name = PConstructor mempty (Label () name)

{-# INLINE anyP #-}
anyP :: (Monoid a) => Pattern a ()
anyP = PAny mempty ()

{-# INLINE tupleP #-}
tupleP :: (Monoid a) => NonEmpty (Pattern a ()) -> Pattern a ()
tupleP = PTuple mempty ()

{-# INLINE funDef #-}
funDef :: (Monoid a) => NonEmpty (Pattern a ()) -> Expression a () -> FunctionDef a ()
funDef = FunctionDef mempty Nothing (With [] ())
