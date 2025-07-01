{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.Dictionaries where

import Control.Monad.RWS (RWS, runRWS)
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, asks, local)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, tell, listen)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List (nub)
import Data.Map.Strict (Map)
import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (supplied)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name)
import Noll.Language
import Noll.Module
import Noll.SystemF.Substitution
import Noll.SystemF.Unification
import Noll.Utils (hashed)

import qualified Data.List.NonEmpty as List1
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment
import Lang.Common.List1 (NonEmpty (..), fromList1)

data DictionaryEnvironment = DictionaryEnvironment
  { dictionaryEnvironmentNames :: Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))
  , dictionaryEnvironmentInstances :: Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
  }
  deriving (Show, Eq, Ord)

overDictionaryEnvironmentNames :: (Environment (Scheme TypeIndex Kind (Type TypeIndex Kind)) -> Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))) -> DictionaryEnvironment -> DictionaryEnvironment
overDictionaryEnvironmentNames f DictionaryEnvironment{..} = DictionaryEnvironment{dictionaryEnvironmentNames = f dictionaryEnvironmentNames, ..}

newtype DictionaryStack a = DictionaryStack { dictionaryStack :: RWS DictionaryEnvironment [Trait (Type TypeIndex Kind)] Int a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader DictionaryEnvironment
    , MonadState Int
    , MonadWriter [Trait (Type TypeIndex Kind)]
    )

runDictionaryStack :: DictionaryEnvironment -> Int -> DictionaryStack a -> (a, Int)
runDictionaryStack e s d = (a, n)
  where
    (a, n, _) = runRWS (dictionaryStack d) e s 

collectTraitsY :: Type TypeIndex Kind -> Name -> DictionaryStack [Trait (Type TypeIndex Kind)]
collectTraitsY u name = do
  env <- asks dictionaryEnvironmentNames
  case Environment.lookup name env of
    Nothing ->
      pure []
    Just (Forall _ [] _) ->
      pure []
    Just (Forall vs ts t) -> do
      sub1 <- foldrM instantiate mempty vs
      r <- tryMatch (apply sub1 t) u
      case r of
        Left{} ->
          error "TODO"
        Right sub2 ->
          pure (apply sub2 (apply sub1 ts))
 where
  instantiate (TypeIndex _ index) acc = do
    var <- supplied (TVariable . TypeIndex KType)
    pure (index `mapsTo` var <> acc)

tryMatch :: Type TypeIndex Kind -> Type TypeIndex Kind -> DictionaryStack (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

-- TODO: rename
mapAlterAll :: (Monad m) => (a -> m (Either e Substitution)) -> Map a (Dictionary (Scheme TypeIndex Kind IndexedType)) -> m [(a, Map Name (Scheme TypeIndex Kind IndexedType))]
mapAlterAll f m = do
  let abc = [fn k v | (k, v) <- Map.toList m]
  def <- sequence abc
  pure (concat def)
 where
  fn k env = do
    s <- f k
    case s of
      Left{} ->
        pure []
      Right sub -> do
        pure [(k, Map.map (applySpecial sub) env)]

applySpecial :: Substitution -> Scheme TypeIndex Kind IndexedType -> Scheme TypeIndex Kind IndexedType
applySpecial sub (Forall _ ts t) = Forall (typeIndexesIn t1 <> typeIndexesIn ts1) ts1 t1
 where
  ts1 = apply sub ts
  t1 = apply sub t

findFirstMatch :: Trait (Type TypeIndex Kind) -> DictionaryStack (Maybe (Type TypeIndex Kind, Map Name (Scheme TypeIndex Kind IndexedType)))
findFirstMatch (Trait name t1) = do
  env <- asks dictionaryEnvironmentInstances
  case Environment.lookup name env of
    Nothing ->
      pure Nothing
    Just env1 -> do
      abc <- mapAlterAll (`tryMatch` t1) env1
      case abc of
        [] ->
          pure Nothing
        (k, v) : _ ->
          pure (Just (k, v))

lookupTraitInstance :: (Monoid a) => Trait (Type TypeIndex Kind) -> DictionaryStack (Maybe (Map Name (Expression a (Type TypeIndex Kind))))
lookupTraitInstance tr@(Trait tn t1) = do
  xx <- findFirstMatch tr
  case xx of
    Nothing ->
      pure Nothing
    Just (a, b) -> do
      let a01 = Map.toList b
      z01 <- forM a01 (uncurry (gork (Trait tn a)))
      let zz1 = Map.fromList z01
      pure (Just zz1)

gork :: (Monoid a) => Trait (Type TypeIndex Kind) -> Name -> Scheme TypeIndex Kind (Type TypeIndex Kind) -> DictionaryStack (Name, Expression a (Type TypeIndex Kind))
gork xx name (Forall _ ts t) = do
  abc <- znorkY xyz ts
  pure (name, abc)
 where
  --  EVariable mempty

  xyz = Label t (name <> "__$instance." <> hashed xx)

bork :: (Monoid a) => Trait (Type TypeIndex Kind) -> DictionaryStack (Expression a (Type TypeIndex Kind))
bork tr@(Trait _ t) = do
  xx <- lookupTraitInstance tr
  case xx of
    Nothing -> do
      tell [tr]
      pure (EPlaceholder mempty (traitType tr) tr)
    Just r ->
      -- TODO
      pure zz
     where
      zz = ERecord mempty (traitType tr) r Nothing

transformScope :: (Monoid a, Data a) => Expression a (Type TypeIndex Kind) -> DictionaryStack (Expression a (Type TypeIndex Kind), [Trait (Type TypeIndex Kind)])
transformScope e = do
  (expr, traits) <- listen (transformY e)
  case nub traits of
    [] ->
      pure (expr, traits)
    tr : trs ->
      pure (ELambda mempty (toPattern <$> (tr :| trs)) expr, traits)
 where
  toPattern tr@(Trait _ t) =
    PPlaceholder mempty (traitType tr) tr

znorkY :: (Monoid a) => Label (Type TypeIndex Kind) -> [Trait (Type TypeIndex Kind)] -> DictionaryStack (Expression a (Type TypeIndex Kind))
znorkY ll@(Label t name) =
  \case
    [] ->
      pure (EVariable mempty ll)
    tr : trs ->
      EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse bork (tr :| trs)
     where
      --      EApplication mempty t (EVariable mempty (Label t name)) <$> traverse bork (tr :| trs)

      t1 = foldType t (traitType <$> (tr : trs))

transformY :: (Monoid a, Data a) => Expression a (Type TypeIndex Kind) -> DictionaryStack (Expression a (Type TypeIndex Kind))
transformY =
  \case
    ERecursiveLet a p e1 e2 -> do
      xx23 <- transformY (ELet a (BPattern a p e1 :| []) e2)
      case xx23 of
        ELet a2 (BPattern _ p2 e8 :| []) e9 ->
          pure (ERecursiveLet a2 p2 e8 e9)
        _ ->
          error "Implementation error"
    ELet a bs e -> do
      (as, traits) <- listen (traverse transformBindingY bs)
      let (ds, es) = List1.unzip as
      let xs = concat (fromList1 (snd <$> as)) -- :: [Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))]
      ELet a (fst <$> as) <$> local (overDictionaryEnvironmentNames (Environment.insertMultiple xs)) (transformY e)
    -- let (ds, es) = NonEmpty.unzip as
    -- let xs = concat (NonEmpty.toList (snd <$> as)) -- :: [Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))]
    -- ELet a (fst <$> as) <$> local (Environment.insertMultiple xs) (transformZ e)
    var@(EVariable a ll@(Label t name)) -> do
      traits <- collectTraitsY t name
      znorkY ll traits
    --    EApplication a t e es -> do
    --      zz <- transformY e
    --      pure (EApplication a t e es)
    --      --EApplication a t
    --      --  <$> transformY e
    --      --  <*> traverse transformY es
    ECompiledMatch a t e cs ->
      ECompiledMatch a t
        <$> transformY e
        <*> traverse transformCompiledClauseY cs
    e ->
      descendM transformY e

traitType :: Trait (Type TypeIndex Kind) -> Type TypeIndex Kind
traitType (Trait name t) = TApplication KTrait (TConstructor (KType `KArrow` KTrait) name) (t :| [])

transformBindingY :: (Monoid a, Data a) => Binding Expression a (Type TypeIndex Kind) -> DictionaryStack (Binding Expression a (Type TypeIndex Kind), [(Name, Scheme TypeIndex Kind (Type TypeIndex Kind))])
transformBindingY =
  \case
    BPattern a var@(PVariable a1 (Label t name)) e
      | Text.isPrefixOf "$fold" name -> do
          body <- transformY e
          pure (BPattern a var body, [])
    BPattern _ var@(PVariable _ (Label t name)) e -> do
      (e1, traits) <- transformScope e
      pure (BPattern mempty var e1, [(name, Forall (typeIndexesIn t) traits t)])
    _ ->
      error "TODO"

transformCompiledClauseY :: (Monoid a, Data a) => CompiledClause a (Type TypeIndex Kind) -> DictionaryStack (CompiledClause a (Type TypeIndex Kind))
transformCompiledClauseY =
  \case
    ECompiledClause lls e ->
      ECompiledClause lls <$> transformY e

transformModuleY :: (Monoid a, Data a) => Module a Kind (Type TypeIndex Kind) -> DictionaryStack (Module a Kind (Type TypeIndex Kind))
transformModuleY = overModuleDefinitionsM (traverse transformDefinitionY)

--traceType t =
--  TApplication
--    KTrait
--    (TConstructor (KType `KArrow` KTrait) "Traceable")
--    (t :| [])

-- Type class?
transformDefinitionY :: (Monoid a, Data a) => Definition a Kind (Type TypeIndex Kind) -> DictionaryStack (Definition a Kind (Type TypeIndex Kind))
transformDefinitionY =
  \case
    DConstant name c ->
      DConstant name <$> transformConstantY c
    DAnnotation a d ->
      DAnnotation a <$> transformDefinitionY d
    d ->
      pure d

transformConstantY :: (Monoid a, Data a) => Constant Expression a (Type TypeIndex Kind) -> DictionaryStack (Constant Expression a (Type TypeIndex Kind))
transformConstantY (Constant a (With _ t) e) = do
  (expr, traits) <- listen (descendM transformY e)
  case nub traits of
    [] ->
      pure (Constant a (With [] t) expr)
    tr : trs ->
      pure
        ( Constant
            a
            (With (tr : trs) t)
            (ELambda mempty (toPattern <$> (tr :| trs)) expr)
        )
 where
  toPattern tr = PPlaceholder mempty (traitType tr) tr
