{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Kind.Constraint.Generation (
  EmitKinds (..),
  KindConstraintsGen (..),
  KindConstraintsGenOutput,
  runKindConstraintsGen,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Definition
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.HasKind (HasKind (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Qualified (..), Trait (..))
import Coal.Language.Type (Parameter (..), Type (..))
import Coal.Language.Type.Kind (Kind (..), tupleConstructorKind)
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.TypeSystem.Kind.Constraint (KindConstraint (..))
import Coal.TypeSystem.Kind.Error (KindError (..))
import Control.Monad.RWS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Extras (Dictionary, Name, concatForM, second, (<$$>), (<>^))
import Extras.Control.Monad.Writer (tellLeft, tellRight)

type KindConstraintsGenOutput = Either KindError KindConstraint

newtype KindConstraintsGen m a = KindConstraintsGen
  { kindConstraintsGenMonad :: RWST (Environment Kind) [KindConstraintsGenOutput] () m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment Kind)
    , MonadWriter [KindConstraintsGenOutput]
    , MonadState ()
    , MonadRWS (Environment Kind) [KindConstraintsGenOutput] ()
    )

runKindConstraintsGen :: (Monad m) => Environment Kind -> KindConstraintsGen m a -> m (a, [KindConstraintsGenOutput])
runKindConstraintsGen env gen = do
  (s, _, w) <- runRWST (kindConstraintsGenMonad gen) env mempty
  pure (s, w)

parameterMap :: [(Name, Kind)] -> Dictionary [Kind]
parameterMap ps = Map.fromListWith (++) (fmap (second pure) ps)

tellTransitive :: (Monad m) => [Kind] -> KindConstraintsGen m ()
tellTransitive [] = pure ()
tellTransitive (k : ks) = forM_ ks $ \ki -> tellRight [KEquality k ki]

tellParameterConstraints :: (Monad m) => [(Name, Kind)] -> KindConstraintsGen m ()
tellParameterConstraints = mapM_ tellTransitive . parameterMap

class EmitKinds k where
  emitKindConstraints :: (Monad m) => k -> KindConstraintsGen m [(Name, Kind)]

instance (EmitKinds k) => EmitKinds [k] where
  emitKindConstraints = concat <$$> traverse emitKindConstraints

instance (EmitKinds k) => EmitKinds (Maybe k) where
  emitKindConstraints = concat <$$> traverse emitKindConstraints

instance (EmitKinds k) => EmitKinds (Map a k) where
  emitKindConstraints = concat <$$> traverse emitKindConstraints

instance (EmitKinds k) => EmitKinds (Set k) where
  emitKindConstraints = emitKindConstraints . Set.toList

instance (EmitKinds k) => EmitKinds (NonEmpty k) where
  emitKindConstraints = emitKindConstraints . NonEmpty.toList

instance EmitKinds (DataConstructor Parameter Kind (Type Parameter Kind)) where
  emitKindConstraints =
    \case
      DataConstructor{..} ->
        emitKindConstraints constructorScheme

instance EmitKinds (Type Parameter Kind) where
  emitKindConstraints =
    \case
      TApplication k t1 t2 -> do
        tellRight [KEquality (kindOf t1) (KArrow (kindOf t2) k)]
        emitKindConstraints t1 <>^ emitKindConstraints t2
      TArrow t1 t2 ->
        emitKindConstraints t1 <>^ emitKindConstraints t2
      TConstructor k "List" -> do
        tellRight [KEquality k (KArrow KType KType)]
        pure []
      TConstructor k con
        | "#Tuple" `Text.isPrefixOf` con -> do
            tellRight [KEquality k (tupleConstructorKind con)]
            pure []
      TConstructor k name -> do
        env <- ask
        case Environment.lookup name env of
          Nothing ->
            tellLeft [ENoTypeConstructor name]
          Just k1 ->
            tellRight [KEquality k k1]
        pure []
      TIntrinsic{} ->
        pure []
      TRecord t ->
        emitKindConstraints t
      TRow row ->
        emitKindConstraints row
      TVariable p ->
        emitKindConstraints p
      TAlias _ ts t ->
        emitKindConstraints ts <>^ emitKindConstraints t

instance EmitKinds (Row Parameter Kind (Type Parameter Kind)) where
  emitKindConstraints =
    \case
      RExtend _ t row ->
        emitKindConstraints t <>^ emitKindConstraints row
      RVariable Parameter{..} ->
        pure
          [ (parameterName, KRow)
          , (parameterName, parameterKind)
          ]
      RNil ->
        pure []

instance EmitKinds (Scheme Parameter Kind (Type Parameter Kind)) where
  emitKindConstraints =
    \case
      Forall{..} -> do
        ps <-
          emitKindConstraints schemeTypeVariables
            <>^ emitKindConstraints schemeTraits
            <>^ emitKindConstraints schemeTypeBody
        tellParameterConstraints ps
        pure ps

instance EmitKinds (Parameter Kind) where
  emitKindConstraints =
    \case
      Parameter{..} ->
        pure [(parameterName, parameterKind)]

instance (EmitKinds t, HasKind t) => EmitKinds (Trait t) where
  emitKindConstraints =
    \case
      Trait{..} -> do
        env <- ask
        case Environment.lookup traitName env of
          Nothing ->
            tellLeft [ENoTrait traitName]
          Just k ->
            tellRight [KEquality k (kindOf traitType `KArrow` KTrait)]
        emitKindConstraints traitType

instance EmitKinds (Module a Kind ()) where
  emitKindConstraints =
    \case
      Module{..} ->
        emitKindConstraints moduleDefinitions

instance EmitKinds (Definition a Kind ()) where
  emitKindConstraints =
    \case
      DType _ _ def ->
        emitKindConstraints def
      DTypeAlias _ _ def ->
        emitKindConstraints def
      DFunction _ _ def ->
        emitKindConstraints def
      DFunctionGroup _ _ FunctionGroupDefinition{..} ->
        concat <$> traverse emitKindConstraints functionGroupDefinitionBranches
      DFold _ _ def ->
        emitKindConstraints def
      DLet _ _ def ->
        emitKindConstraints def
      DImport{} ->
        pure []
      DNamespaceImport{} ->
        pure []
      DTrait _ _ def ->
        emitKindConstraints def
      DInstance _ def ->
        emitKindConstraints def

instance EmitKinds (Qualified (Type Parameter Kind)) where
  emitKindConstraints =
    \case
      With traits t ->
        emitKindConstraints traits <>^ emitKindConstraints t

instance EmitKinds (Label (Type Parameter Kind)) where
  emitKindConstraints =
    \case
      Label{..} ->
        emitKindConstraints labelTag

instance EmitKinds (Expression a Kind ()) where
  emitKindConstraints =
    \case
      EAnnotation _ t e ->
        emitKindConstraints t <>^ emitKindConstraints e
      EApplication _ () e es ->
        emitKindConstraints e <>^ emitKindConstraints es
      ELambda _ ps e ->
        emitKindConstraints ps <>^ emitKindConstraints e
      ELet _ bs e ->
        emitKindConstraints bs <>^ emitKindConstraints e
      ERecursiveLet _ p e1 e2 ->
        emitKindConstraints p
          <>^ emitKindConstraints e1
          <>^ emitKindConstraints e2
      EIf _ () e1 e2 e3 ->
        emitKindConstraints e1
          <>^ emitKindConstraints e2
          <>^ emitKindConstraints e3
      ERecord _ () d e ->
        emitKindConstraints d
          <>^ emitKindConstraints e
      EListCons _ () e1 e2 ->
        emitKindConstraints e1
          <>^ emitKindConstraints e2
      EListLiteral _ () es ->
        emitKindConstraints es
      ETuple _ () es ->
        emitKindConstraints es
      EMatch _ () e cs ->
        emitKindConstraints e
          <>^ emitKindConstraints cs
      ELambdaMatch _ () cs ->
        emitKindConstraints cs
      ECompiledMatch _ () e cs ->
        emitKindConstraints e
          <>^ emitKindConstraints cs
      EFold _ () es cs ->
        emitKindConstraints es
          <>^ emitKindConstraints cs
      ESelect _ _ e ->
        emitKindConstraints e
      EFocus _ _ _ _ e1 e2 ->
        emitKindConstraints e1
          <>^ emitKindConstraints e2
      EFFICall _ _ _ es e ->
        --        emitKindConstraints t
        emitKindConstraints es <>^ emitKindConstraints e
      EDoBlock _ is -> do
        concatForM is $
          \(x, y) ->
            emitKindConstraints x
              <>^ emitKindConstraints y
      _ ->
        pure []

instance EmitKinds (Clause a Kind ()) where
  emitKindConstraints =
    \case
      EClause _ p cs ->
        emitKindConstraints p
          <>^ emitKindConstraints cs

instance EmitKinds (Choice Expression a Kind ()) where
  emitKindConstraints =
    \case
      CPlain _ gs e ->
        emitKindConstraints gs
          <>^ emitKindConstraints e

instance EmitKinds (Guard Expression a Kind ()) where
  emitKindConstraints =
    \case
      CGuard e ->
        emitKindConstraints e

instance EmitKinds (CompiledClause a Kind ()) where
  emitKindConstraints =
    \case
      ECompiledClause _ _ e ->
        emitKindConstraints e

instance EmitKinds (Binding Expression a Kind ()) where
  emitKindConstraints =
    \case
      BPattern _ p e ->
        emitKindConstraints p <>^ emitKindConstraints e
      BFunction _ _ ps e ->
        emitKindConstraints ps <>^ emitKindConstraints e

instance EmitKinds (Pattern a Kind ()) where
  emitKindConstraints =
    \case
      PAnnotation _ t p ->
        emitKindConstraints t <>^ emitKindConstraints p
      PConstructor _ _ ps ->
        emitKindConstraints ps
      PRecord _ () d p ->
        emitKindConstraints d <>^ emitKindConstraints p
      PListCons _ () p1 p2 ->
        emitKindConstraints p1 <>^ emitKindConstraints p2
      PListLiteral _ () ps ->
        emitKindConstraints ps
      PTuple _ () ps ->
        emitKindConstraints ps
      POr _ () p1 p2 ->
        emitKindConstraints p1 <>^ emitKindConstraints p2
      PAs _ _ p ->
        emitKindConstraints p
      _ ->
        pure []

instance EmitKinds (TypeDefinition a Kind t) where
  emitKindConstraints =
    \case
      TypeDefinition{..} ->
        emitKindConstraints typeDefinitionParameters
          <>^ emitKindConstraints typeDefinitionConstructors

instance EmitKinds (FunctionDefinition a Kind ()) where
  emitKindConstraints =
    \case
      FunctionDefinition{..} -> do
        ps <-
          emitKindConstraints functionDefinitionAnnotation
            <>^ emitKindConstraints functionDefinitionPatterns
            <>^ emitKindConstraints functionDefinitionExpression
        tellParameterConstraints ps
        pure ps

instance EmitKinds (LetDefinition a Kind ()) where
  emitKindConstraints =
    \case
      LetDefinition{..} -> do
        ps <-
          emitKindConstraints letDefinitionAnnotation
            <>^ emitKindConstraints letDefinitionExpression
        tellParameterConstraints ps
        pure ps

instance EmitKinds (TraitDefinition a Kind) where
  emitKindConstraints =
    \case
      TraitDefinition{..} -> do
        ps1 <-
          emitKindConstraints traitDefinitionConstraints
            <>^ emitKindConstraints traitDefinitionParameter
            <>^ emitKindConstraints (Trait traitDefinitionTraitName traitDefinitionParameter)
        forM_ traitDefinitionInterface $
          \(TraitDefinitionInterfaceEntry _ Forall{..}) -> do
            ps2 <-
              emitKindConstraints schemeTypeVariables
                <>^ emitKindConstraints schemeTraits
                <>^ emitKindConstraints schemeTypeBody
            tellParameterConstraints (ps1 <> ps2)
        pure []

instance EmitKinds (InstanceDefinition a Kind ()) where
  emitKindConstraints =
    \case
      InstanceDefinition{..} -> do
        ps1 <- emitKindConstraints (Trait instanceDefinitionTraitName instanceDefinitionType)
        ps2 <- emitKindConstraints instanceDefinitionConstraints
        tellParameterConstraints (ps1 <> ps2)
        forM_ instanceDefinitionImplementations emitKindConstraints
        pure []

instance EmitKinds (FoldDefinition a Kind ()) where
  emitKindConstraints =
    \case
      FoldDefinition{..} -> do
        ps <-
          emitKindConstraints foldDefinitionAnnotation
            <>^ emitKindConstraints foldDefinitionClauses
        tellParameterConstraints ps
        pure ps

instance EmitKinds (AliasDefinition a Kind) where
  emitKindConstraints =
    \case
      AliasDefinition{..} -> do
        ps <-
          emitKindConstraints aliasDefinitionParameters
            <>^ emitKindConstraints aliasDefinitionType
        tellParameterConstraints ps
        pure ps
