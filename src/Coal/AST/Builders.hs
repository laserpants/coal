module Coal.AST.Builders (
  matchE,
  varE,
  varE',
  letE,
  letE',
  ifE,
  applicationE,
  applicationE',
  lambdaE,
  lambdaE',
  lambda1E,
  lambdaAnyE,
  lambdaAnyE',
  selectE,
  tupleE,
  literalE,
  literalBoolE,
  plainClauseE,
  opAndE,
  varP,
  constructorP,
  anyP,
  tupleP,
) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name)

{-# INLINE matchE #-}
matchE :: (Monoid a) => Expression a k () -> NonEmpty (Clause a k ()) -> Expression a k ()
matchE = EMatch mempty ()

{-# INLINE varE #-}
varE :: (Monoid a) => Name -> Expression a k ()
varE = EVariable mempty . label

{-# INLINE varE' #-}
varE' :: a -> Name -> Expression a k ()
varE' a = EVariable a . label

{-# INLINE ifE #-}
ifE :: (Monoid a) => Expression a k () -> Expression a k () -> Expression a k () -> Expression a k ()
ifE = EIf mempty ()

{-# INLINE letE #-}
letE :: (Monoid a) => Name -> Expression a k () -> Expression a k () -> Expression a k ()
letE name = ERecursiveLet mempty (PVariable mempty (label name))

{-# INLINE letE' #-}
letE' :: a -> Name -> Expression a k () -> Expression a k () -> Expression a k ()
letE' a name = ERecursiveLet a (PVariable a (label name))

{-# INLINE applicationE #-}
applicationE :: (Monoid a) => Expression a k () -> NonEmpty (Expression a k ()) -> Expression a k ()
applicationE = EApplication mempty ()

{-# INLINE applicationE' #-}
applicationE' :: a -> Expression a k () -> NonEmpty (Expression a k ()) -> Expression a k ()
applicationE' a = EApplication a ()

{-# INLINE lambdaE #-}
lambdaE :: (Monoid a) => NonEmpty (Pattern a k ()) -> Expression a k () -> Expression a k ()
lambdaE = ELambda mempty

{-# INLINE lambdaE' #-}
lambdaE' :: a -> NonEmpty (Pattern a k ()) -> Expression a k () -> Expression a k ()
lambdaE' = ELambda

{-# INLINE lambda1E #-}
lambda1E :: (Monoid a) => Name -> Expression a k () -> Expression a k ()
lambda1E var = ELambda mempty (PVariable mempty (label var) :| [])

{-# INLINE lambdaAnyE #-}
lambdaAnyE :: (Monoid a) => Expression a k () -> Expression a k ()
lambdaAnyE = ELambda mempty (PAny mempty () :| [])

{-# INLINE lambdaAnyE' #-}
lambdaAnyE' :: a -> Expression a k () -> Expression a k ()
lambdaAnyE' a = ELambda a (PAny a () :| [])

{-# INLINE selectE #-}
selectE :: (Monoid a) => Name -> Expression a k () -> Expression a k ()
selectE = ESelect mempty . label

{-# INLINE tupleE #-}
tupleE :: (Monoid a) => NonEmpty (Expression a k ()) -> Expression a k ()
tupleE = ETuple mempty ()

{-# INLINE literalE #-}
literalE :: (Monoid a) => Primitive -> Expression a k ()
literalE = ELiteral mempty

{-# INLINE literalBoolE #-}
literalBoolE :: (Monoid a) => Bool -> Expression a k ()
literalBoolE = ELiteral mempty . LBool

{-# INLINE opAndE #-}
opAndE :: (Monoid a) => Expression a k ()
opAndE = EOperator mempty () OLogicalAnd

{-# INLINE plainClauseE #-}
plainClauseE :: (Monoid a) => Pattern a k () -> Expression a k () -> Clause a k ()
plainClauseE p e = EClause mempty p (CPlain mempty [] e :| [])

{-# INLINE label #-}
label :: Name -> Label ()
label = Label ()

{-# INLINE varP #-}
varP :: (Monoid a) => Name -> Pattern a k ()
varP = PVariable mempty . Label ()

{-# INLINE constructorP #-}
constructorP :: (Monoid a) => Name -> [Pattern a k ()] -> Pattern a k ()
constructorP name = PConstructor mempty (Label () name)

{-# INLINE anyP #-}
anyP :: (Monoid a) => Pattern a k ()
anyP = PAny mempty ()

{-# INLINE tupleP #-}
tupleP :: (Monoid a) => NonEmpty (Pattern a k ()) -> Pattern a k ()
tupleP = PTuple mempty ()
