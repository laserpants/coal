{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Compiler.Dictionaries where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ask, asks, local)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (MonadWriter, runWriterT, tell)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Debug.Trace
import Lang.Common.Environment (Environment (..))
import Lang.Common.List1 (List1, NonEmpty ((:|)), (<|))
import Lang.Common.Supply (Supply (..), supplied)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name)
import Noll.Language
import Noll.Language.Expression (Expression (..))
import Noll.Language.Trait
import Noll.Language.Type
import Noll.Language.Type.Kind
import Noll.Module
import Noll.SystemF.Substitution
import Noll.SystemF.Unification
import Noll.Utils (hashed)

import qualified Data.List.NonEmpty as List1
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

collectTraitsY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Type TypeIndex Kind ->
  Name ->
  m [Trait (Type TypeIndex Kind)]
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
        Left x ->
          pure []
        -- TODO
        -- error (show u)
        Right sub2 ->
          pure (apply sub2 (apply sub1 ts))
 where
  instantiate (TypeIndex k index) acc = do
    var <- supplied (TVariable . TypeIndex KType)
    pure (index `mapsTo` var <> acc)

tryMatch ::
  (MonadState Int m) =>
  Type TypeIndex Kind ->
  Type TypeIndex Kind ->
  m (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

-- TODO: rename
-- mapAlterAll :: (Monad m, Ord k) => (k -> m (Maybe Substitution)) -> Map k (Environment (Scheme TypeIndex Kind IndexedType)) -> m [(k, Map Name (Scheme TypeIndex Kind IndexedType))]
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
applySpecial sub (Forall vs ts t) = Forall (typeIndexesIn t1 <> typeIndexesIn ts1) ts1 t1
 where
  ts1 = apply sub ts
  t1 = apply sub t

-- nork :: Substitution -> TypeIndex Kind -> Maybe (TypeIndex Kind)
-- nork = undefined

findFirstMatch ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Trait (Type TypeIndex Kind) ->
  m (Maybe (Type TypeIndex Kind, Map Name (Scheme TypeIndex Kind IndexedType)))
findFirstMatch (Trait nnn t1) = do
  env <- asks dictionaryEnvironmentInstances
  case Environment.lookup nnn env of
    Nothing ->
      pure Nothing
    Just env1 -> do
      abc <- mapAlterAll test env1
      case abc of
        [] ->
          pure Nothing
        [(k, v)] ->
          pure (Just (k, v))
 where
  --   test :: (Monad m) => Type TypeIndex Kind -> m (Maybe Substitution) -- (Environment (Scheme TypeIndex Kind IndexedType))
  test t = tryMatch t t1

--    case x of
--      Left{} ->
--        pure Nothing
--      Right sub ->
--        pure (Just sub)

lookupTraitInstance ::
  ( Monoid a
  , Data a
  , Show a
  , MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Trait (Type TypeIndex Kind) ->
  --  m (Maybe (Environment (Scheme TypeIndex Kind IndexedType)))
  m (Maybe (Map Name (Expression a (Type TypeIndex Kind))))
lookupTraitInstance tr@(Trait tn t1) = do
  xx <- findFirstMatch tr
  case xx of
    Nothing ->
      pure Nothing
    Just (a, b) -> do
      let a01 = Map.toList b
      z01 <- forM a01 (\(z, b) -> gork (Trait tn a) z b)
      let zz1 = Map.fromList z01
      pure (Just zz1)

-- pure (Just zz1) -- (Just (Map.mapWithKey (gork (Trait tn a)) b))
-- where
--  zoop =
--    Map.fromList
--      []

gork ::
  ( Monoid a
  , Data a
  , Show a
  , MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Trait (Type TypeIndex Kind) ->
  Name ->
  Scheme TypeIndex Kind (Type TypeIndex Kind) ->
  m (Name, Expression a (Type TypeIndex Kind))
gork xx name (Forall _ ts t) = do
  abc <- znorkY xyz ts
  pure (name, abc)
 where
  --  EVariable mempty

  xyz = Label t (name <> "__$instance." <> hashed xx)

bork ::
  ( Monoid a
  , Data a
  , Show a
  , MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  ) =>
  Trait (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind))
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

transformScope ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Monoid a
  , Show a
  , Data a
  ) =>
  Expression a (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind), [Trait (Type TypeIndex Kind)])
transformScope e = do
  (expr, traits) <- runWriterT (transformY e)
  case nub traits of
    [] ->
      pure (expr, traits)
    tr : trs ->
      pure (ELambda mempty (toPattern <$> (tr :| trs)) expr, traits)
 where
  toPattern tr@(Trait _ t) =
    PDictionary mempty (traitType tr) tr

znorkY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Monoid a
  , Show a
  , Data a
  ) =>
  Label (Type TypeIndex Kind) ->
  [Trait (Type TypeIndex Kind)] ->
  m (Expression a (Type TypeIndex Kind))
znorkY ll@(Label t name) =
  \case
    [] ->
      pure (EVariable mempty ll)
    tr : trs ->
      EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse bork (tr :| trs)
     where
      --      EApplication mempty t (EVariable mempty (Label t name)) <$> traverse bork (tr :| trs)

      t1 = foldType t (traitType <$> (tr : trs))

transformY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Monoid a
  , Show a
  , Data a
  ) =>
  Expression a (Type TypeIndex Kind) ->
  m (Expression a (Type TypeIndex Kind))
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
      (as, traits) <- runWriterT (traverse transformBindingY bs)
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

transformBindingY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Show a
  , Monoid a
  , Data a
  ) =>
  Binding Expression a (Type TypeIndex Kind) ->
  m (Binding Expression a (Type TypeIndex Kind), [(Name, Scheme TypeIndex Kind (Type TypeIndex Kind))])
transformBindingY =
  \case
    BPattern a var@(PVariable a1 (Label t name)) e
      | Text.isPrefixOf "$fold" name -> do
          body <- transformY e
          pure (BPattern a var body, [])
    BPattern _ var@(PVariable _ (Label t name)) e -> do
      (e1, traits) <- transformScope e
      pure (BPattern mempty var e1, [(name, Forall (typeIndexesIn t) traits t)])

transformCompiledClauseY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Data a
  , Monoid a
  , Show a
  ) =>
  CompiledClause a (Type TypeIndex Kind) ->
  m (CompiledClause a (Type TypeIndex Kind))
transformCompiledClauseY =
  \case
    ECompiledClause lls e ->
      ECompiledClause lls <$> transformY e

transformModuleY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Data a
  , Monoid a
  , Show a
  ) =>
  Module a Kind (Type TypeIndex Kind) ->
  m (Module a Kind (Type TypeIndex Kind))
transformModuleY = overModuleDefinitionsM (traverse transformDefinitionY)

--

traceType t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Traceable")
    (t :| [])

yy :: Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))
yy =
  Environment.fromList
    [
      ( "trace"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Traceable" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
      )
    ,
      ( "pair_to_string"
      , Forall
          (Set.fromList [TypeIndex KType 0, TypeIndex KType 1] :: Set (TypeIndex Kind))
          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
          , Trait "Traceable" (TVariable (TypeIndex KType 1))
          ]
          (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
      )
    ,
      ( "list_to_string"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
          ]
          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
      )
    ,
      ( "from_int32"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0))]
          (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
      )
    ,
      ( "greater_than"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
      )
    ,
      ( "less_than_or_equal_to"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
      )
    ,
      ( "compare"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
      )
    ,
      ( "from_list"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])))
      )
    ,
      ( "in_range"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          ( TIntrinsic (IRecord (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil))))
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ,
      ( "sort"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
      )
    ]

-- zz :: Map IndexedType (Map String (Function Expression () IndexedType))
-- zz =
--  Map.fromList
--    [
--      ( TIntrinsic IString
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                (With [] (TIntrinsic IString))
--                (PVariable () (Label (TIntrinsic IString) "s") :| [])
--                (EVariable () (Label (TIntrinsic IString) "s"))
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic IInt32
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                (With [] (TIntrinsic IString))
--                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "int32_to_string"))
--                    (EVariable () (Label (TIntrinsic IInt32) "n") :| [])
--                )
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                ( With
--                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--                    , Trait "Traceable" (TVariable (TypeIndex KType 1))
--                    ]
--                    (TIntrinsic IString)
--                )
--                (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString) "pair_to_string"))
--                    (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
--                )
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                ( With
--                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--                    ]
--                    (TIntrinsic IString)
--                )
--                (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString) "list_to_string"))
--                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
--                )
--            )
--          ]
--      )
--    ]

xx :: Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
xx =
  Environment.fromList
    [
      ( "Numeric"
      , Map.fromList
          [
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
          ,
            ( TIntrinsic INat
            , Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic INat)
                  )
                ]
            )
          ]
      )
    ,
      ( "Ordered"
      , Map.fromList
          [
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "compare"
                  , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          ]
      )
    ,
      ( "Traceable"
      , Map.fromList
          [
            ( TIntrinsic IString
            , Map.fromList
                [
                  ( "trace"
                  , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ,
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "trace"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ,
            ( TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
            , Map.fromList
                [
                  ( "trace"
                  , Forall
                      (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
                      [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                      , Trait "Traceable" (TVariable (TypeIndex KType 1))
                      ]
                      (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ,
            ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
            , Map.fromList
                [
                  ( "trace"
                  , Forall
                      (Set.fromList [TypeIndex KType 0])
                      [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                      ]
                      (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ]
      )
    ]

-- Type class?
transformDefinitionY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Monoid a
  , Show a
  , Data a
  ) =>
  Definition a Kind (Type TypeIndex Kind) ->
  m (Definition a Kind (Type TypeIndex Kind))
transformDefinitionY =
  \case
    DConstant name c ->
      DConstant name <$> transformConstantY c
    DAnnotation a d ->
      DAnnotation a <$> transformDefinitionY d
    d ->
      pure d

transformConstantY ::
  ( MonadReader DictionaryEnvironment m
  , MonadState Int m
  , MonadWriter [Trait (Type TypeIndex Kind)] m
  , Monoid a
  , Show a
  , Data a
  ) =>
  Constant Expression a (Type TypeIndex Kind) ->
  m (Constant Expression a (Type TypeIndex Kind))
transformConstantY (Constant a u@(With _ t) e) = do
  -- e1 <- transformScope e
  (expr, traits) <- runWriterT (descendM transformY e)
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
  toPattern tr@(Trait _ t) =
    PDictionary mempty (traitType tr) tr
