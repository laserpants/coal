{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoTypeSystem.Kind.Constraint.Generation (
  ProtoKindInferenceError (..),
  ProtoKindConstraintsGen (..),
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
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (Parameter (..), Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Control.Monad.RWS
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Dictionary, Name, concatForM, second, (<$$>), (<>^))
import Extras.Control.Monad.Writer (tellLeft, tellRight)

data ProtoKindInferenceError
  = ProtoENoTypeConstructor Name
  | ProtoECannotUnifyKinds
  | ProtoEInfiniteKind
  deriving (Show, Eq, Ord, Read)

type ProtoKindConstraintsGenOutput = Either ProtoKindInferenceError ProtoKindConstraint

newtype ProtoKindConstraintsGen a = ProtoKindConstraintsGen
  { protoOkindConstraintsGenMonad :: RWS (Environment Kind) [ProtoKindConstraintsGenOutput] () a
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

parameterMap :: [(Name, Kind)] -> Dictionary [Kind]
parameterMap ps = Map.fromListWith (++) (fmap (second pure) ps)

tellTransitive :: [Kind] -> ProtoKindConstraintsGen ()
tellTransitive [] = pure ()
tellTransitive (k : ks) = forM_ ks $ \ki -> tellRight [ProtoKEquality k ki]

tellParameterConstraints :: [(Name, Kind)] -> ProtoKindConstraintsGen ()
tellParameterConstraints = mapM_ tellTransitive . parameterMap

class ProtoEmitKinds k where
  protoOemitKindConstraints :: k -> ProtoKindConstraintsGen [(Name, Kind)]

instance (ProtoEmitKinds k) => ProtoEmitKinds [k] where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (ProtoEmitKinds k) => ProtoEmitKinds (Maybe k) where
  protoOemitKindConstraints = concat <$$> traverse protoOemitKindConstraints

instance (ProtoEmitKinds k) => ProtoEmitKinds (Map n k) where
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

instance (ProtoEmitKinds k) => ProtoEmitKinds (Trait k) where
  protoOemitKindConstraints =
    \case
      Trait{..} ->
        protoOemitKindConstraints traitType

instance ProtoEmitKinds (ProtoModule a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      ProtoModule{..} ->
        protoOemitKindConstraints protoOmoduleDefinitions

instance ProtoEmitKinds (ProtoDefinition a Kind (Type Parameter Kind)) where
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

instance (ProtoEmitKinds t) => ProtoEmitKinds (With t) where
  protoOemitKindConstraints =
    \case
      With traits t ->
        protoOemitKindConstraints traits <>^ protoOemitKindConstraints t

instance ProtoEmitKinds (Label (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      Label{..} ->
        protoOemitKindConstraints labelTag

instance ProtoEmitKinds (Expression a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      EAnnotation _ t e ->
        protoOemitKindConstraints t <>^ protoOemitKindConstraints e
      EApplication _ t e es ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints e
          <>^ protoOemitKindConstraints es
      ELambda _ ps e ->
        protoOemitKindConstraints ps <>^ protoOemitKindConstraints e
      ELet _ bs e ->
        protoOemitKindConstraints bs <>^ protoOemitKindConstraints e
      ERecursiveLet _ p e1 e2 ->
        protoOemitKindConstraints p
          <>^ protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
      EVariable _ ll ->
        protoOemitKindConstraints ll
      EConstructor _ ll ->
        protoOemitKindConstraints ll
      ELiteral{} ->
        pure []
      EIf _ t e1 e2 e3 ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
          <>^ protoOemitKindConstraints e3
      EOperator _ t _ ->
        protoOemitKindConstraints t
      ERecord _ t d e ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints d
          <>^ protoOemitKindConstraints e
      EListCons _ t e1 e2 ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
      EListLiteral _ t es ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints es
      ETuple _ t es ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints es
      EMatch _ t e cs ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints e
          <>^ protoOemitKindConstraints cs
      ELambdaMatch _ t cs ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints cs
      ECompiledMatch _ t e cs ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints e
          <>^ protoOemitKindConstraints cs
      EFold _ t es cs ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints es
          <>^ protoOemitKindConstraints cs
      ESelect _ ll e ->
        protoOemitKindConstraints ll
          <>^ protoOemitKindConstraints e
      EFocus _ _ ll1 ll2 e1 e2 ->
        protoOemitKindConstraints ll1
          <>^ protoOemitKindConstraints ll2
          <>^ protoOemitKindConstraints e1
          <>^ protoOemitKindConstraints e2
      ETraitInstance _ t trait ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints trait
      EFFICall _ t _ es e ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints es
          <>^ protoOemitKindConstraints e
      EDoBlock _ is -> do
        concatForM is $
          \(x, y) ->
            protoOemitKindConstraints x
              <>^ protoOemitKindConstraints y

instance ProtoEmitKinds (Clause a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      EClause _ p cs ->
        protoOemitKindConstraints p
          <>^ protoOemitKindConstraints cs

instance ProtoEmitKinds (Choice Expression a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      CPlain _ gs e ->
        protoOemitKindConstraints gs
          <>^ protoOemitKindConstraints e

instance ProtoEmitKinds (Guard Expression a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      CGuard e ->
        protoOemitKindConstraints e

instance ProtoEmitKinds (CompiledClause a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      ECompiledClause _ lls e ->
        protoOemitKindConstraints lls
          <>^ protoOemitKindConstraints e

instance ProtoEmitKinds (Binding Expression a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      BPattern _ p e ->
        protoOemitKindConstraints p <>^ protoOemitKindConstraints e
      BFunction _ _ ps e ->
        protoOemitKindConstraints ps <>^ protoOemitKindConstraints e

instance ProtoEmitKinds (Pattern a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      PAnnotation _ t p ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints p
      PAny _ t ->
        protoOemitKindConstraints t
      PVariable _ ll ->
        protoOemitKindConstraints ll
      PConstructor _ ll ps ->
        protoOemitKindConstraints ll
          <>^ protoOemitKindConstraints ps
      PInteger _ t _ ->
        protoOemitKindConstraints t
      PLiteral{} ->
        pure []
      PRecord _ t d p ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints d
          <>^ protoOemitKindConstraints p
      PListCons _ t p1 p2 ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints p1
          <>^ protoOemitKindConstraints p2
      PListLiteral _ t ps ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints ps
      PTuple _ t ps ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints ps
      POr _ t p1 p2 ->
        protoOemitKindConstraints t
          <>^ protoOemitKindConstraints p1
          <>^ protoOemitKindConstraints p2
      PAs _ ll p ->
        protoOemitKindConstraints ll
          <>^ protoOemitKindConstraints p
      PShorthand _ ll ->
        protoOemitKindConstraints ll
      PAtVariable _ ll ->
        protoOemitKindConstraints ll
      PNamedFold _ _ ll ->
        protoOemitKindConstraints ll
      PTraitInstance _ t trait ->
        protoOemitKindConstraints t <>^ protoOemitKindConstraints trait

instance ProtoEmitKinds (ProtoTypeDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoTypeDefinition{..} ->
        protoOemitKindConstraints protoOtypeDefinitionParameters
          <>^ protoOemitKindConstraints protoOtypeDefinitionConstructors

-- E.g.,
--
-- fun add3(x : int32, y : int32, z : int32) : int32 =
--   x + y + z
instance ProtoEmitKinds (ProtoFunctionDefinition a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      ProtoFunctionDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOfunctionDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOfunctionDefinitionType
            <>^ protoOemitKindConstraints protoOfunctionDefinitionPatterns
            <>^ protoOemitKindConstraints protoOfunctionDefinitionExpression
        tellParameterConstraints ps
        pure ps

-- E.g.,
--
-- let
--   limit = 5 : int32
instance ProtoEmitKinds (ProtoLetDefinition a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      ProtoLetDefinition{..} -> do
        ps <-
          protoOemitKindConstraints protoOletDefinitionAnnotation
            <>^ protoOemitKindConstraints protoOletDefinitionType
            <>^ protoOemitKindConstraints protoOletDefinitionExpression
        tellParameterConstraints ps
        pure ps

-- E.g.,
--
-- trait Applicative<f> with Functor<f> {
--   pure : a -> f<a>
--   ap : f<a -> b> -> f<a> -> f<b>
-- }
instance ProtoEmitKinds (ProtoTraitDefinition a Kind) where
  protoOemitKindConstraints =
    \case
      ProtoTraitDefinition{..} -> do
        ps1 <-
          protoOemitKindConstraints protoOtraitDefinitionConstraints
            <>^ protoOemitKindConstraints protoOtraitDefinitionParameter
        forM_ protoOtraitDefinitionInterface $
          \(_, Forall{..}) -> do
            ps2 <-
              protoOemitKindConstraints schemeTypeVariables
                <>^ protoOemitKindConstraints schemeTraits
                <>^ protoOemitKindConstraints schemeTypeBody
            tellParameterConstraints (ps1 <> ps2)
        pure []

-- E.g.,
--
-- instance Ordered<bool> {
--   compare
--     | (False, True) = LessThan
--     | (True, False) = GreaterThan
--     | (_, _)        = EqualTo
-- }
instance ProtoEmitKinds (ProtoInstanceDefinition a Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      ProtoInstanceDefinition{..} ->
        undefined

-- E.g.,
--
-- fold length
--   | x :: @xs => 1 + xs
--   | [] => 0
instance ProtoEmitKinds (ProtoFoldDefinition a Kind (Type Parameter Kind)) where
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
