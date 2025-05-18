{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.DictionaryElimination (EliminateDictionariesTransformContext (..)) where

import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (transform)
import Data.Map.Strict (Map)
import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Compiler.TraitTransform (parameterized)
import Noll.Compiler.Transform (flattenLambda)
import Noll.Language
import Noll.Module (Module (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition (Definition (..))
import Noll.Module.Function (Function (..))
import Noll.Utils (hashed)

import qualified Lang.Common.List1 as List1

class EliminateDictionariesTransformContext a where
  eliminateDictionaries :: a -> a

instance (EliminateDictionariesTransformContext a) => EliminateDictionariesTransformContext [a] where
  eliminateDictionaries = fmap eliminateDictionaries

instance (EliminateDictionariesTransformContext a) => EliminateDictionariesTransformContext (List1 a) where
  eliminateDictionaries = fmap eliminateDictionaries

instance (EliminateDictionariesTransformContext a) => EliminateDictionariesTransformContext (Map k a) where
  eliminateDictionaries = fmap eliminateDictionaries

instance (Monoid a, Data a) => EliminateDictionariesTransformContext (Module a Kind (Type TypeIndex Kind)) where
  eliminateDictionaries =
    \case
      Module p ns d ->
        Module p ns (eliminateDictionaries d)

instance (Monoid a, Data a) => EliminateDictionariesTransformContext (Definition a Kind (Type TypeIndex Kind)) where
  eliminateDictionaries =
    \case
      DAnnotation u d ->
        DAnnotation u (eliminateDictionaries d)
      DFunction name (Function a (With ts t) ps e) ->
        DFunction name (Function a (With ts t) ps (eliminateDictionaries e))
      DConstant name (Constant a (With ts t) e) ->
        DConstant name (Constant a (With ts t) (eliminateDictionaries e))
      d ->
        d

instance (Monoid a, Data a) => EliminateDictionariesTransformContext (Expression a (Type TypeIndex Kind)) where
  eliminateDictionaries = transform eliminateDictionariesExpr

eliminateDictionariesExpr :: (Monoid a, Data a) => Expression a (Type TypeIndex Kind) -> Expression a (Type TypeIndex Kind)
eliminateDictionariesExpr =
  \case
    EDictionaryLambda a ts e ->
      flattenLambda (ELambda a (pvars ts) e)
    EDictionaryApplication a t (Label t1 name) ts es ->
      EApplication a t (EVariable mempty (Label (foldType t1 (dictionaryType <$> ts)) name)) (evars ts `List1.appendList` es)
    e ->
      e

setType :: Type TypeIndex Kind -> Expression a (Type TypeIndex Kind) -> Expression a (Type TypeIndex Kind)
setType t =
  \case
    EVariable a (Label _ name) ->
      EVariable a (Label t name)
    e ->
      -- TODO
      e

{-# INLINE pvars #-}
pvars :: (Functor f, Monoid a) => f (Trait (Type TypeIndex Kind)) -> f (Pattern a (Type TypeIndex Kind))
pvars = fmap (translateTrait PVariable)

{-# INLINE evars #-}
evars :: (Functor f, Monoid a) => f (Trait (Type TypeIndex Kind)) -> f (Expression a (Type TypeIndex Kind))
evars = fmap (translateTrait EVariable)

translateTrait :: (Monoid a) => (a -> Label (Type TypeIndex Kind) -> t) -> Trait (Type TypeIndex Kind) -> t
translateTrait ctor trait@(Trait tname _) = ctor mempty (Label (dictionaryType trait) (name <> hashed trait))
 where
  name
    | parameterized trait = "$dict."
    | otherwise = tname <> "__$instance."

dictionaryType :: Trait (Type TypeIndex Kind) -> Type TypeIndex Kind
dictionaryType (Trait name t) =
  TApplication
    KTrait
    (TConstructor (KArrow KType KTrait) name)
    (t :| [])
