{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoTypeSystem.Kind.Constraint.Generation (
  ProtoEmitKinds (..),
  ProtoKindConstraintsGen (..),
  ProtoKindConstraintsGenOutput,
  runProtoKindConstraintsGen,
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Expression (Clause (..), CompiledClause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.HasKind (HasKind (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Qualified (..), Trait (..))
import Coal.Language.Type (Parameter (..), Type (..))
import Coal.Language.Type.Kind (Kind (..), tupleConstructorKind)
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..))
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

type ProtoKindConstraintsGenOutput = Either ProtoKindError ProtoKindConstraint

newtype ProtoKindConstraintsGen m a = ProtoKindConstraintsGen
  { protoOkindConstraintsGenMonad :: RWST (Environment Kind) [ProtoKindConstraintsGenOutput] () m a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment Kind)
    , MonadWriter [ProtoKindConstraintsGenOutput]
    , MonadState ()
    , MonadRWS (Environment Kind) [ProtoKindConstraintsGenOutput] ()
    )

runProtoKindConstraintsGen :: (Monad m) => Environment Kind -> ProtoKindConstraintsGen m a -> m (a, [ProtoKindConstraintsGenOutput])
runProtoKindConstraintsGen env gen = do
  (s, _, w) <- runRWST (protoOkindConstraintsGenMonad gen) env mempty
  pure (s, w)

parameterMap :: [(Name, Kind)] -> Dictionary [Kind]
parameterMap ps = Map.fromListWith (++) (fmap (second pure) ps)

tellTransitive :: (Monad m) => [Kind] -> ProtoKindConstraintsGen m ()
tellTransitive [] = pure ()
tellTransitive (k : ks) = forM_ ks $ \ki -> tellRight [ProtoKEquality k ki]

tellParameterConstraints :: (Monad m) => [(Name, Kind)] -> ProtoKindConstraintsGen m ()
tellParameterConstraints = mapM_ tellTransitive . parameterMap

class ProtoEmitKinds k where
  protoOemitKindConstraints :: (Monad m) => k -> ProtoKindConstraintsGen m [(Name, Kind)]

instance (ProtoEmitKinds k) => ProtoEmitKinds [k] where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (ProtoEmitKinds k) => ProtoEmitKinds (Maybe k) where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (ProtoEmitKinds k) => ProtoEmitKinds (Map a k) where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (ProtoEmitKinds k) => ProtoEmitKinds (Set k) where
  protoOemitKindConstraints = protoOemitKindConstraints . Set.toList

instance (ProtoEmitKinds k) => ProtoEmitKinds (NonEmpty k) where
  protoOemitKindConstraints = protoOemitKindConstraints . NonEmpty.toList

instance ProtoEmitKinds (DataConstructor Parameter Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      DataConstructor{..} ->
        protoOemitKindConstraints constructorScheme

instance ProtoEmitKinds (Type Parameter Kind) where
  protoOemitKindConstraints =
    \case
      TApplication k t1 t2 -> do
        tellRight [ProtoKEquality (kindOf t1) (KArrow (kindOf t2) k)]
        protoOemitKindConstraints t1 <>^ protoOemitKindConstraints t2
      TArrow t1 t2 ->
        protoOemitKindConstraints t1 <>^ protoOemitKindConstraints t2
      TConstructor k "List" -> do
        tellRight [ProtoKEquality k (KArrow KType KType)]
        pure []
      TConstructor k con
        | "#Tuple" `Text.isPrefixOf` con -> do
            tellRight [ProtoKEquality k (tupleConstructorKind con)]
            pure []
      TConstructor k name -> do
        env <- ask
        case Environment.lookup name env of
          Nothing ->
            tellLeft [ProtoENoTypeConstructor name]
          Just k1 ->
            tellRight [ProtoKEquality k k1]
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

instance ProtoEmitKinds (Row Parameter Kind (Type Parameter Kind)) where
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

instance ProtoEmitKinds (Scheme Parameter Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      Forall{..} -> do
        ps <-
          protoOemitKindConstraints schemeTypeVariables
            <>^ protoOemitKindConstraints schemeTraits
            <>^ protoOemitKindConstraints schemeTypeBody
        tellParameterConstraints ps
        pure ps

instance ProtoEmitKinds (Parameter Kind) where
  protoOemitKindConstraints =
    \case
      Parameter{..} ->
        pure [(parameterName, parameterKind)]

instance (ProtoEmitKinds t, HasKind t) => ProtoEmitKinds (Trait t) where
  protoOemitKindConstraints =
    \case
      Trait{..} -> do
        env <- ask
        case Environment.lookup traitName env of
          Nothing ->
            tellLeft [ProtoENoTrait traitName]
          Just k ->
            tellRight [ProtoKEquality k (kindOf traitType `KArrow` KTrait)]
        protoOemitKindConstraints traitType

instance ProtoEmitKinds (ProtoModule a Kind ()) where
  protoOemitKindConstraints =
    \case
      ProtoModule{..} ->
        protoOemitKindConstraints protoOmoduleDefinitions

instance ProtoEmitKinds (ProtoDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      ProtoDType _ _ def ->
        protoOemitKindConstraints def
      ProtoDTypeAlias _ _ def ->
        protoOemitKindConstraints def
      ProtoDFunction _ _ def ->
        protoOemitKindConstraints def
      ProtoDFunctionGroup _ _ defs ->
        concat <$> traverse protoOemitKindConstraints defs
      ProtoDFold _ _ def ->
        protoOemitKindConstraints def
      ProtoDLet _ _ def ->
        protoOemitKindConstraints def
      ProtoDImport{} ->
        pure []
      ProtoDQualifiedImport{} ->
        pure []
      ProtoDTrait _ _ def ->
        protoOemitKindConstraints def
      ProtoDInstance _ def ->
        protoOemitKindConstraints def

instance ProtoEmitKinds (Qualified (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      With traits t ->
        protoOemitKindConstraints traits <>^ protoOemitKindConstraints t

instance ProtoEmitKinds (Label (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      Label{..} ->
        protoOemitKindConstraints labelTag

instance ProtoEmitKinds (Expression a Kind ()) where
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

instance ProtoEmitKinds (Clause a Kind ()) where
  protoOemitKindConstraints =
    \case
      EClause _ p cs ->
        protoOemitKindConstraints p
          <>^ protoOemitKindConstraints cs

instance ProtoEmitKinds (Choice Expression a Kind ()) where
  protoOemitKindConstraints =
    \case
      CPlain _ gs e ->
        protoOemitKindConstraints gs
          <>^ protoOemitKindConstraints e

instance ProtoEmitKinds (Guard Expression a Kind ()) where
  protoOemitKindConstraints =
    \case
      CGuard e ->
        protoOemitKindConstraints e

instance ProtoEmitKinds (CompiledClause a Kind ()) where
  protoOemitKindConstraints =
    \case
      ECompiledClause _ _ e ->
        protoOemitKindConstraints e

instance ProtoEmitKinds (Binding Expression a Kind ()) where
  protoOemitKindConstraints =
    \case
      BPattern _ p e ->
        protoOemitKindConstraints p <>^ protoOemitKindConstraints e
      BFunction _ _ ps e ->
        protoOemitKindConstraints ps <>^ protoOemitKindConstraints e

instance ProtoEmitKinds (Pattern a Kind ()) where
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

instance ProtoEmitKinds (ProtoTypeDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoTypeDefinition{..} ->
        protoOemitKindConstraints protoOtypeDefinitionParameters
          <>^ protoOemitKindConstraints protoOtypeDefinitionConstructors

instance ProtoEmitKinds (ProtoFunctionDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      ProtoFunctionDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOfunctionDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOfunctionDefinitionPatterns
            <>^ protoOemitKindConstraints protoOfunctionDefinitionExpression
        tellParameterConstraints ps
        pure ps

instance ProtoEmitKinds (ProtoLetDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      ProtoLetDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOletDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOletDefinitionExpression
        tellParameterConstraints ps
        pure ps

instance ProtoEmitKinds (ProtoTraitDefinition a Kind) where
  protoOemitKindConstraints =
    \case
      ProtoTraitDefinition{..} -> do
        ps1 <-
          protoOemitKindConstraints protoOtraitDefinitionConstraints
            <>^ protoOemitKindConstraints protoOtraitDefinitionParameter
            <>^ protoOemitKindConstraints (Trait protoOtraitDefinitionTraitName protoOtraitDefinitionParameter)
        forM_ protoOtraitDefinitionInterface $
          \(ProtoTraitDefinitionInterfaceEntry _ Forall{..}) -> do
            ps2 <-
              protoOemitKindConstraints schemeTypeVariables
                <>^ protoOemitKindConstraints schemeTraits
                <>^ protoOemitKindConstraints schemeTypeBody
            tellParameterConstraints (ps1 <> ps2)
        pure []

instance ProtoEmitKinds (ProtoInstanceDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      ProtoInstanceDefinition{..} -> do
        ps1 <- protoOemitKindConstraints (Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType)
        ps2 <- protoOemitKindConstraints protoOinstanceDefinitionConstraints
        tellParameterConstraints (ps1 <> ps2)
        forM_ protoOinstanceDefinitionImplementations protoOemitKindConstraints
        pure []

instance ProtoEmitKinds (ProtoFoldDefinition a Kind ()) where
  protoOemitKindConstraints =
    \case
      ProtoFoldDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOfoldDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOfoldDefinitionClauses
        tellParameterConstraints ps
        pure ps

instance ProtoEmitKinds (ProtoAliasDefinition a Kind) where
  protoOemitKindConstraints =
    \case
      ProtoAliasDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOaliasDefinitionParameters
            <>^ protoOemitKindConstraints protoOaliasDefinitionType
        tellParameterConstraints ps
        pure ps
