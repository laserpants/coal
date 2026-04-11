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
import Coal.Language.Module
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
  { protoOkindConstraintsGenMonad :: RWST (Environment Kind) [KindConstraintsGenOutput] () m a
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
  (s, _, w) <- runRWST (protoOkindConstraintsGenMonad gen) env mempty
  pure (s, w)

parameterMap :: [(Name, Kind)] -> Dictionary [Kind]
parameterMap ps = Map.fromListWith (++) (fmap (second pure) ps)

tellTransitive :: (Monad m) => [Kind] -> KindConstraintsGen m ()
tellTransitive [] = pure ()
tellTransitive (k : ks) = forM_ ks $ \ki -> tellRight [KEquality k ki]

tellParameterConstraints :: (Monad m) => [(Name, Kind)] -> KindConstraintsGen m ()
tellParameterConstraints = mapM_ tellTransitive . parameterMap

class EmitKinds k where
  protoOemitKindConstraints :: (Monad m) => k -> KindConstraintsGen m [(Name, Kind)]

instance (EmitKinds k) => EmitKinds [k] where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (EmitKinds k) => EmitKinds (Maybe k) where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (EmitKinds k) => EmitKinds (Map a k) where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (EmitKinds k) => EmitKinds (Set k) where
  protoOemitKindConstraints = protoOemitKindConstraints . Set.toList

instance (EmitKinds k) => EmitKinds (NonEmpty k) where
  protoOemitKindConstraints = protoOemitKindConstraints . NonEmpty.toList

instance EmitKinds (DataConstructor Parameter Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      DataConstructor{..} ->
        protoOemitKindConstraints constructorScheme

instance EmitKinds (Type Parameter Kind) where
  protoOemitKindConstraints =
    \case
      TApplication k t1 t2 -> do
        tellRight [KEquality (kindOf t1) (KArrow (kindOf t2) k)]
        protoOemitKindConstraints t1 <>^ protoOemitKindConstraints t2
      TArrow t1 t2 ->
        protoOemitKindConstraints t1 <>^ protoOemitKindConstraints t2
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
        protoOemitKindConstraints t
      TRow row ->
        protoOemitKindConstraints row
      TVariable p ->
        protoOemitKindConstraints p
      TAlias _ ts t ->
        protoOemitKindConstraints ts <>^ protoOemitKindConstraints t

instance EmitKinds (Row Parameter Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      RExtend _ t row ->
        protoOemitKindConstraints t <>^ protoOemitKindConstraints row
      RVariable Parameter{..} ->
        pure
          [ (parameterName, KRow)
          , (parameterName, parameterKind)
          ]
      RNil ->
        pure []

instance EmitKinds (Scheme Parameter Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      Forall{..} -> do
        ps <-
          protoOemitKindConstraints schemeTypeVariables
            <>^ protoOemitKindConstraints schemeTraits
            <>^ protoOemitKindConstraints schemeTypeBody
        tellParameterConstraints ps
        pure ps

instance EmitKinds (Parameter Kind) where
  protoOemitKindConstraints =
    \case
      Parameter{..} ->
        pure [(parameterName, parameterKind)]

instance (EmitKinds t, HasKind t) => EmitKinds (Trait t) where
  protoOemitKindConstraints =
    \case
      Trait{..} -> do
        env <- ask
        case Environment.lookup traitName env of
          Nothing ->
            tellLeft [ENoTrait traitName]
          Just k ->
            tellRight [KEquality k (kindOf traitType `KArrow` KTrait)]
        protoOemitKindConstraints traitType

instance EmitKinds (Module a Kind ()) where
  protoOemitKindConstraints =
    \case
      Module{..} ->
        protoOemitKindConstraints protoOmoduleDefinitions

instance EmitKinds (Definition a Kind ()) where
  protoOemitKindConstraints =
    \case
      DType _ _ def ->
        protoOemitKindConstraints def
      DTypeAlias _ _ def ->
        protoOemitKindConstraints def
      DFunction _ _ def ->
        protoOemitKindConstraints def
      DFunctionGroup _ _ defs ->
        concat <$> traverse protoOemitKindConstraints defs
      DFold _ _ def ->
        protoOemitKindConstraints def
      DLet _ _ def ->
        protoOemitKindConstraints def
      DImport{} ->
        pure []
      DNamespaceImport{} ->
        pure []
      DTrait _ _ def ->
        protoOemitKindConstraints def
      DInstance _ def ->
        protoOemitKindConstraints def

instance EmitKinds (Qualified (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      With traits t ->
        protoOemitKindConstraints traits <>^ protoOemitKindConstraints t

instance EmitKinds (Label (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      Label{..} ->
        protoOemitKindConstraints labelTag

instance EmitKinds (Expression a Kind ()) where
  protoOemitKindConstraints =
    \case
      EAnnotation _ t e ->
        protoOemitKindConstraints t <>^ protoOemitKindConstraints e
      EApplication _ () e es ->
        protoOemitKindConstraints e <>^ protoOemitKindConstraints es
      ELambda _ ps e ->
        protoOemitKindConstraints ps <>^ protoOemitKindConstraints e
      ELet _ bs e ->
        protoOemitKindConstraints bs <>^ protoOemitKindConstraints e
      ERecursiveLet _ p e1 e2 ->
        protoOemitKindConstraints p
          <>^ protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
      EIf _ () e1 e2 e3 ->
        protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
          <>^ protoOemitKindConstraints e3
      ERecord _ () d e ->
        protoOemitKindConstraints d
          <>^ protoOemitKindConstraints e
      EListCons _ () e1 e2 ->
        protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
      EListLiteral _ () es ->
        protoOemitKindConstraints es
      ETuple _ () es ->
        protoOemitKindConstraints es
      EMatch _ () e cs ->
        protoOemitKindConstraints e
          <>^ protoOemitKindConstraints cs
      ELambdaMatch _ () cs ->
        protoOemitKindConstraints cs
      ECompiledMatch _ () e cs ->
        protoOemitKindConstraints e
          <>^ protoOemitKindConstraints cs
      EFold _ () es cs ->
        protoOemitKindConstraints es
          <>^ protoOemitKindConstraints cs
      ESelect _ _ e ->
        protoOemitKindConstraints e
      EFocus _ _ _ _ e1 e2 ->
        protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
      EFFICall _ t _ es e ->
        --        protoOemitKindConstraints t
        protoOemitKindConstraints es <>^ protoOemitKindConstraints e
      EDoBlock _ is -> do
        concatForM is $
          \(x, y) ->
            protoOemitKindConstraints x
              <>^ protoOemitKindConstraints y
      _ ->
        pure []

instance EmitKinds (Clause a Kind ()) where
  protoOemitKindConstraints =
    \case
      EClause _ p cs ->
        protoOemitKindConstraints p
          <>^ protoOemitKindConstraints cs

instance EmitKinds (Choice Expression a Kind ()) where
  protoOemitKindConstraints =
    \case
      CPlain _ gs e ->
        protoOemitKindConstraints gs
          <>^ protoOemitKindConstraints e

instance EmitKinds (Guard Expression a Kind ()) where
  protoOemitKindConstraints =
    \case
      CGuard e ->
        protoOemitKindConstraints e

instance EmitKinds (CompiledClause a Kind ()) where
  protoOemitKindConstraints =
    \case
      ECompiledClause _ _ e ->
        protoOemitKindConstraints e

instance EmitKinds (Binding Expression a Kind ()) where
  protoOemitKindConstraints =
    \case
      BPattern _ p e ->
        protoOemitKindConstraints p <>^ protoOemitKindConstraints e
      BFunction _ _ ps e ->
        protoOemitKindConstraints ps <>^ protoOemitKindConstraints e

instance EmitKinds (Pattern a Kind ()) where
  protoOemitKindConstraints =
    \case
      PAnnotation _ t p ->
        protoOemitKindConstraints t <>^ protoOemitKindConstraints p
      PConstructor _ _ ps ->
        protoOemitKindConstraints ps
      PRecord _ () d p ->
        protoOemitKindConstraints d <>^ protoOemitKindConstraints p
      PListCons _ () p1 p2 ->
        protoOemitKindConstraints p1 <>^ protoOemitKindConstraints p2
      PListLiteral _ () ps ->
        protoOemitKindConstraints ps
      PTuple _ () ps ->
        protoOemitKindConstraints ps
      POr _ () p1 p2 ->
        protoOemitKindConstraints p1 <>^ protoOemitKindConstraints p2
      PAs _ _ p ->
        protoOemitKindConstraints p
      _ ->
        pure []

instance EmitKinds (TypeDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      TypeDefinition{..} ->
        protoOemitKindConstraints protoOtypeDefinitionParameters
          <>^ protoOemitKindConstraints protoOtypeDefinitionConstructors

instance EmitKinds (FunctionDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      FunctionDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOfunctionDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOfunctionDefinitionPatterns
            <>^ protoOemitKindConstraints protoOfunctionDefinitionExpression
        tellParameterConstraints ps
        pure ps

instance EmitKinds (LetDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      LetDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOletDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOletDefinitionExpression
        tellParameterConstraints ps
        pure ps

instance EmitKinds (TraitDefinition a Kind) where
  protoOemitKindConstraints =
    \case
      TraitDefinition{..} -> do
        ps1 <-
          protoOemitKindConstraints protoOtraitDefinitionConstraints
            <>^ protoOemitKindConstraints protoOtraitDefinitionParameter
            <>^ protoOemitKindConstraints (Trait protoOtraitDefinitionTraitName protoOtraitDefinitionParameter)
        forM_ protoOtraitDefinitionInterface $
          \(TraitDefinitionInterfaceEntry _ Forall{..}) -> do
            ps2 <-
              protoOemitKindConstraints schemeTypeVariables
                <>^ protoOemitKindConstraints schemeTraits
                <>^ protoOemitKindConstraints schemeTypeBody
            tellParameterConstraints (ps1 <> ps2)
        pure []

instance EmitKinds (InstanceDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      InstanceDefinition{..} -> do
        ps1 <- protoOemitKindConstraints (Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType)
        ps2 <- protoOemitKindConstraints protoOinstanceDefinitionConstraints
        tellParameterConstraints (ps1 <> ps2)
        forM_ protoOinstanceDefinitionImplementations protoOemitKindConstraints
        pure []

instance EmitKinds (FoldDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      FoldDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOfoldDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOfoldDefinitionClauses
        tellParameterConstraints ps
        pure ps

instance EmitKinds (AliasDefinition a Kind) where
  protoOemitKindConstraints =
    \case
      AliasDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOaliasDefinitionParameters
            <>^ protoOemitKindConstraints protoOaliasDefinitionType
        tellParameterConstraints ps
        pure ps
