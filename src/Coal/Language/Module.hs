{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module (
  Module (..),
  Export (..),
  overModuleDefinitions,
  overModuleDefinitionsM,
  fromDefinitionList,
  modulePathName,
  principalPath,
  qualified,
  importedPaths,
  toProtoModule,
  fromProtoModule,
  module Coal.Language.Module.Definition,
  module Coal.Language.Module.Definition.Function,
  module Coal.Language.Module.Definition.Constant,
  module Coal.Language.Module.Definition.Fold,
  module Coal.Language.Module.Definition.Alias,
  module Coal.Language.Module.Definition.Trait,
  module Coal.Language.Module.Definition.Type,
  module Coal.Language.Module.Definition.Instance,
  module Coal.Language.Module.Import,
  module Coal.Language.Module.Export,
) where

import Coal.Common.Label (Label (..))
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.Expression
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module.Definition (Definition (..), Path (..), definitionName, importPath)
import Coal.Language.Module.Definition.Alias
import Coal.Language.Module.Definition.Constant
import Coal.Language.Module.Definition.Fold
import Coal.Language.Module.Definition.Function
import Coal.Language.Module.Definition.Instance
import Coal.Language.Module.Definition.Trait
import Coal.Language.Module.Definition.Type
import Coal.Language.Module.Export
import Coal.Language.Module.Import
import Coal.Language.Module.Path (principalPath)
import Coal.Language.Pattern
import Coal.Language.Trait
import Coal.Language.Type
import Coal.Language.Type.Kind
import Coal.Language.Type.Row
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Extras (Name, Over)

data Module a k t = Module
  { modulePath :: Path
  , moduleExports :: [Export a]
  , moduleDefinitions :: [Definition a k t]
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    , Data
    , Typeable
    )

overModuleDefinitions :: Over (Module a k t) [Definition a k t]
overModuleDefinitions fn (Module path names defs) = Module path names (fn defs)

overModuleDefinitionsM :: (Monad m) => ([Definition a k t] -> m [Definition a k t]) -> Module a k t -> m (Module a k t)
overModuleDefinitionsM fn (Module path names defs) = Module path names <$> fn defs

{-# INLINE insertDefinition #-}
insertDefinition :: Definition a k t -> Module a k t -> Module a k t
insertDefinition def = overModuleDefinitions (def :)

fromDefinitionList :: Path -> [Export a] -> [Definition a k t] -> Module a k t
fromDefinitionList path exports = foldr insertDefinition (Module path exports mempty)

{-# INLINE modulePathName #-}
modulePathName :: Module a k t -> Name
modulePathName = principalPath . modulePath

qualified :: Name -> Path -> Name
qualified name path = principalPath path <> "." <> name

importedPaths :: Module a k t -> [(a, Path)]
importedPaths = mapMaybe importPath . moduleDefinitions

--

toProtoModuleExportList :: [Export a] -> ModuleExportList a
toProtoModuleExportList [] = ExportAll
toProtoModuleExportList es = Exports es

fromProtoModuleExportList :: ModuleExportList a -> [Export a]
fromProtoModuleExportList ExportAll = []
fromProtoModuleExportList (Exports es) = es

toProtoTypeDefinition :: TypeDefinition -> ProtoTypeDefinition a () t
toProtoTypeDefinition TypeDefinition{..} =
  ProtoTypeDefinition
    { protoOtypeDefinitionParameters = typeDefinitionParameters
    , protoOtypeDefinitionConstructors = typeDefinitionConstructors
    }

fromProtoTypeDefinition :: ProtoTypeDefinition a Kind t -> TypeDefinition
fromProtoTypeDefinition ProtoTypeDefinition{..} =
  TypeDefinition
    { typeDefinitionParameters = fmap fromProtoParameter protoOtypeDefinitionParameters
    , typeDefinitionConstructors = fmap fromProtoTypeDefinitionConstructors protoOtypeDefinitionConstructors
    }

fromProtoParameter :: Parameter Kind -> Parameter ()
fromProtoParameter (Parameter _ name) = Parameter () name

fromProtoTypeDefinitionConstructors :: DataConstructor Parameter Kind (Type Parameter Kind) -> DataConstructor Parameter () (Type Parameter ())
fromProtoTypeDefinitionConstructors DataConstructor{..} =
  DataConstructor
    { constructorName = constructorName
    , constructorArity = constructorArity
    , constructorScheme = fromProtoScheme constructorScheme
    }

fromProtoScheme :: Scheme Parameter Kind (Type Parameter Kind) -> Scheme Parameter () (Type Parameter ())
fromProtoScheme Forall{..} =
  Forall
    { schemeTypeVariables = Set.map fromProtoParameter schemeTypeVariables
    , schemeTraits = Set.map fromProtoSchemeTrait schemeTraits
    , schemeTypeBody = fromTypeKind schemeTypeBody
    }

fromProtoSchemeTrait :: Trait (Type Parameter Kind) -> Trait (Type Parameter ())
fromProtoSchemeTrait = fmap fromTypeKind

fromTypeKind :: Type Parameter Kind -> Type Parameter ()
fromTypeKind =
  \case
    TApplication _ t1 t2 ->
      TApplication () (fromTypeKind t1) (fromTypeKind t2)
    TArrow t1 t2 ->
      TArrow (fromTypeKind t1) (fromTypeKind t2)
    TConstructor _ name ->
      TConstructor () name
    TIntrinsic i ->
      TIntrinsic i
    TRecord t ->
      TRecord (fromTypeKind t)
    TRow r ->
      TRow (fromTypeKindRow r)
    TVariable (Parameter _ name) ->
      TVariable (Parameter () name)
    TAlias name ts t ->
      TAlias name (fromTypeKind <$> ts) (fromTypeKind t)

fromTypeKindRow :: Row Parameter Kind (Type Parameter Kind) -> Row Parameter () (Type Parameter ())
fromTypeKindRow =
  \case
    RExtend name t r ->
      RExtend name (fromTypeKind t) (fromTypeKindRow r)
    RVariable (Parameter _ name) ->
      RVariable (Parameter () name)
    RNil ->
      RNil

toProtoTraitDefinition :: a -> Name -> TraitDefinition () -> ProtoDefinition a () t
toProtoTraitDefinition a name TraitDefinition{..} =
  ProtoDTrait
    a
    name
    ( ProtoTraitDefinition
        { protoOtraitDefinitionMetadata = a
        , protoOtraitDefinitionTraitName = name
        , protoOtraitDefinitionConstraints = traitDefinitionRequired
        , protoOtraitDefinitionParameter = traitDefinitionParameter
        , protoOtraitDefinitionInterface = fmap toInterfaceEntry traitDefinitionMethods
        }
    )

toInterfaceEntry :: (Name, Scheme Parameter () (Type Parameter ())) -> ProtoTraitDefinitionInterfaceEntry ()
toInterfaceEntry (name, scheme) = ProtoTraitDefinitionInterfaceEntry name scheme

fromProtoTraitDefinition :: ProtoTraitDefinition a Kind -> TraitDefinition ()
fromProtoTraitDefinition ProtoTraitDefinition{..} =
  TraitDefinition
    { traitDefinitionRequired = fmap (fmap fromProtoParameter) protoOtraitDefinitionConstraints
    , traitDefinitionParameter = fromProtoParameter protoOtraitDefinitionParameter
    , traitDefinitionMethods = fmap fromInterfaceEntry protoOtraitDefinitionInterface
    }

fromInterfaceEntry :: ProtoTraitDefinitionInterfaceEntry Kind -> (Name, Scheme Parameter () (Type Parameter ()))
fromInterfaceEntry (ProtoTraitDefinitionInterfaceEntry name scheme) = (name, fromProtoScheme scheme)

toProtoInstanceDefinition :: a -> Name -> InstanceDefinition Definition a Kind t -> ProtoDefinition a () t
toProtoInstanceDefinition a name InstanceDefinition{..} =
  ProtoDInstance
    a
    ( ProtoInstanceDefinition
        { protoOinstanceDefinitionMetadata = a
        , protoOinstanceDefinitionTraitName = name
        , protoOinstanceDefinitionConstraints = fmap (fmap fromProtoExpression3) instanceDefinitionParameters
        , protoOinstanceDefinitionType = instanceDefinitionType
        , protoOinstanceDefinitionImplementations = fmap toProtoModuleDefinition instanceDefinitionEntries
        }
    )

fromProtoExpression3 :: Type Parameter () -> Parameter ()
fromProtoExpression3 =
  \case
    TVariable p ->
      p
    _ ->
      error "TODO"

fromProtoInstanceDefinition :: a -> ProtoInstanceDefinition a Kind t -> Definition a Kind t
fromProtoInstanceDefinition a ProtoInstanceDefinition{..} =
  DInstance
    a
    protoOinstanceDefinitionTraitName
    ( InstanceDefinition
        { instanceDefinitionParameters = fmap (fmap fromProtoExpression4) protoOinstanceDefinitionConstraints
        , instanceDefinitionType = fromTypeKind protoOinstanceDefinitionType
        , instanceDefinitionEntries = fmap fromProtoModuleDefinition protoOinstanceDefinitionImplementations
        }
    )

fromProtoExpression4 :: Parameter Kind -> Type Parameter ()
fromProtoExpression4 (Parameter _ name) = TVariable (Parameter () name)

toProtoAliasDefinition :: AliasDefinition -> ProtoAliasDefinition a ()
toProtoAliasDefinition AliasDefinition{..} =
  ProtoAliasDefinition
    { protoOaliasDefinitionParameters = aliasDefinitionParameters
    , protoOaliasDefinitionType = aliasDefinitionType
    }

fromProtoAliasDefinition :: ProtoAliasDefinition a Kind -> AliasDefinition
fromProtoAliasDefinition ProtoAliasDefinition{..} =
  AliasDefinition
    { aliasDefinitionParameters = fmap fromProtoParameter protoOaliasDefinitionParameters
    , aliasDefinitionType = ooo protoOaliasDefinitionType
    }

ooo :: Type Parameter Kind -> Type Parameter ()
ooo =
  \case
    TApplication _ t1 t2 ->
      TApplication () (ooo t1) (ooo t2)
    TArrow t1 t2 ->
      TArrow (ooo t1) (ooo t2)
    TConstructor _ name ->
      TConstructor () name
    TIntrinsic i ->
      TIntrinsic i
    TRecord t ->
      TRecord (ooo t)
    TRow r ->
      TRow (oooRow r)
    TVariable (Parameter _ name) ->
      TVariable (Parameter () name)
    TAlias name ts t ->
      TAlias name (fmap ooo ts) (ooo t)

oooRow :: Row Parameter Kind (Type Parameter Kind) -> Row Parameter () (Type Parameter ())
oooRow =
  \case
    RExtend name t r ->
      RExtend name (ooo t) (oooRow r)
    RVariable (Parameter _ name) ->
      RVariable (Parameter () name)
    RNil ->
      RNil

toProtoFold :: a -> FoldDefinition a t -> ProtoFoldDefinition a () t
toProtoFold a FoldDefinition{..} =
  ProtoFoldDefinition
    { protoOfoldDefinitionMetadata = a
    , protoOfoldDefinitionAnnotation = foldDefinitionType
    , protoOfoldDefinitionClauses = foldDefinitionClauses
    }

fromProtoFold :: ProtoFoldDefinition a Kind t -> FoldDefinition a t
fromProtoFold ProtoFoldDefinition{..} =
  FoldDefinition
    { foldDefinitionType = fmap (fmap fromTypeKind) protoOfoldDefinitionAnnotation
    , foldDefinitionClauses = fmap mmz protoOfoldDefinitionClauses
    }

mmz :: Clause a Kind t -> Clause a () t
mmz EClause{..} =
  EClause
    { clauseMetadata = clauseMetadata
    , clausePattern = fromProtoPattern clausePattern
    , clauseChoices = fmap mmzo clauseChoices
    }

mmz2 :: CompiledClause a Kind t -> CompiledClause a () t
mmz2 ECompiledClause{..} =
  ECompiledClause
    { compiledClauseMetadata = compiledClauseMetadata
    , compiledClauseSegments = compiledClauseSegments
    , compiledClauseExpression = fromProtoExpression compiledClauseExpression
    }

mmzo :: Choice Expression a Kind t -> Choice Expression a () t
mmzo =
  \case
    CPlain a gs e ->
      CPlain a (fmap fromProtoExpressionzo gs) (fromProtoExpression e)

fromProtoExpressionzo :: Guard Expression a Kind t -> Guard Expression a () t
fromProtoExpressionzo =
  \case
    CGuard e ->
      CGuard (fromProtoExpression e)

toProtoFunction :: FunctionDefinition a t -> ProtoFunctionDefinition a () t
toProtoFunction FunctionDefinition{..} =
  ProtoFunctionDefinition
    { protoOfunctionDefinitionMetadata = functionDefinitionMetadata
    , protoOfunctionDefinitionAnnotation = functionDefinitionAnnotation
    , protoOfunctionDefinitionType = functionDefinitionType
    , protoOfunctionDefinitionPatterns = functionDefinitionPatterns
    , protoOfunctionDefinitionExpression = functionDefinitionExpression
    }

fromProtoFunction :: ProtoFunctionDefinition a Kind t -> FunctionDefinition a t
fromProtoFunction ProtoFunctionDefinition{..} =
  FunctionDefinition
    { functionDefinitionMetadata = protoOfunctionDefinitionMetadata
    , functionDefinitionAnnotation = fmap (fmap fromTypeKind) protoOfunctionDefinitionAnnotation
    , functionDefinitionType = protoOfunctionDefinitionType
    , functionDefinitionPatterns = fmap fromProtoPattern protoOfunctionDefinitionPatterns
    , functionDefinitionExpression = fromProtoExpression protoOfunctionDefinitionExpression
    }

fromProtoPattern :: Pattern a Kind t -> Pattern a () t
fromProtoPattern =
  \case
    PAnnotation a t p ->
      PAnnotation a (ooo t) (fromProtoPattern p)
    PAny a t ->
      PAny a t
    PVariable a ll ->
      PVariable a ll
    PConstructor a ll ps ->
      PConstructor a ll (fmap fromProtoPattern ps)
    PInteger a t i ->
      PInteger a t i
    PLiteral a p ->
      PLiteral a p
    PRecord a t d m ->
      PRecord a t (fmap fromProtoPattern d) (fmap fromProtoPattern m)
    PListCons a t p1 p2 ->
      PListCons a t (fromProtoPattern p1) (fromProtoPattern p2)
    PListLiteral a t ps ->
      PListLiteral a t (fmap fromProtoPattern ps)
    PTuple a t ps ->
      PTuple a t (fmap fromProtoPattern ps)
    POr a t p1 p2 ->
      POr a t (fromProtoPattern p1) (fromProtoPattern p2)
    PAs a ll p ->
      PAs a ll (fromProtoPattern p)
    PShorthand a ll ->
      PShorthand a ll
    PAtVariable a ll ->
      PAtVariable a ll
    PNamedFold a name ll ->
      PNamedFold a name ll
    PTraitInstance a t tr ->
      PTraitInstance a t tr

toProtoLetDefinition :: ConstantDefinition a t -> ProtoLetDefinition a () t
toProtoLetDefinition ConstantDefinition{..} =
  ProtoLetDefinition
    { protoOletDefinitionMetadata = constantDefinitionMetadata
    , protoOletDefinitionAnnotation = constantDefinitionAnnotation
    , protoOletDefinitionType = constantDefinitionType
    , protoOletDefinitionExpression = constantDefinitionExpression
    }

fromProtoLetDefinition :: ProtoLetDefinition a Kind t -> ConstantDefinition a t
fromProtoLetDefinition ProtoLetDefinition{..} =
  ConstantDefinition
    { constantDefinitionMetadata = protoOletDefinitionMetadata
    , constantDefinitionAnnotation = fmap (fmap fromTypeKind) protoOletDefinitionAnnotation
    , constantDefinitionType = protoOletDefinitionType
    , constantDefinitionExpression = fromProtoExpression protoOletDefinitionExpression
    }

fromProtoExpression :: Expression a Kind t -> Expression a () t
fromProtoExpression =
  \case
    EAnnotation a t e ->
      EAnnotation a (ooo t) (fromProtoExpression e)
    EApplication a t e es ->
      EApplication a t (fromProtoExpression e) (fmap fromProtoExpression es)
    ELambda a ps e ->
      ELambda a (fmap fromProtoPattern ps) (fromProtoExpression e)
    ELet a bs e ->
      ELet a (fmap fromProtoBinding bs) (fromProtoExpression e)
    ERecursiveLet a p e1 e2 ->
      ERecursiveLet a (fromProtoPattern p) (fromProtoExpression e1) (fromProtoExpression e2)
    EVariable a ll ->
      EVariable a ll
    EConstructor a ll ->
      EConstructor a ll
    ELiteral a p ->
      ELiteral a p
    EIf a t e1 e2 e3 ->
      EIf a t (fromProtoExpression e1) (fromProtoExpression e2) (fromProtoExpression e3)
    EOperator a t op ->
      EOperator a t op
    ERecord a t d m ->
      ERecord a t (fmap fromProtoExpression d) (fmap fromProtoExpression m)
    EListCons a t e1 e2 ->
      EListCons a t (fromProtoExpression e1) (fromProtoExpression e2)
    EListLiteral a t es ->
      EListLiteral a t (fmap fromProtoExpression es)
    ETuple a t es ->
      ETuple a t (fmap fromProtoExpression es)
    EMatch a t e cs ->
      EMatch a t (fromProtoExpression e) (fmap mmz cs)
    ELambdaMatch a t cs ->
      ELambdaMatch a t (fmap mmz cs)
    ECompiledMatch a t e cs ->
      ECompiledMatch a t (fromProtoExpression e) (fmap mmz2 cs)
    EFold a t es cs ->
      EFold a t (fmap fromProtoExpression es) (fmap mmz cs)
    ESelect a ll e ->
      ESelect a ll (fromProtoExpression e)
    EFocus a name ll1 ll2 e1 e2 ->
      EFocus a name ll1 ll2 (fromProtoExpression e1) (fromProtoExpression e2)
    ETraitInstance a t tr ->
      ETraitInstance a t tr
    EFFICall a t (Label s name) es e ->
      EFFICall a t (Label (ooo s) name) (fmap fromProtoExpression es) (fromProtoExpression e)
    EDoBlock a is ->
      EDoBlock a (fmap fromProtoInstr is)

fromProtoInstr :: (Pattern a Kind t, Expression a Kind t) -> (Pattern a () t, Expression a () t)
fromProtoInstr (p, e) = (fromProtoPattern p, fromProtoExpression e)

fromProtoBinding :: Binding Expression a Kind t -> Binding Expression a () t
fromProtoBinding =
  \case
    BPattern a p e ->
      BPattern a (fromProtoPattern p) (fromProtoExpression e)
    BFunction a name ps e ->
      BFunction a name (fmap fromProtoPattern ps) (fromProtoExpression e)

toProtoModuleDefinition :: Definition a Kind t -> ProtoDefinition a () t
toProtoModuleDefinition =
  \case
    DType a name def ->
      ProtoDType a name (toProtoTypeDefinition def)
    DFunction a name (def :| []) _ ->
      ProtoDFunction a name (toProtoFunction def)
    DFunction a name defs _ ->
      ProtoDFunctionGroup a name (toProtoFunction <$> NonEmpty.toList defs)
    DConstant a name def _ ->
      ProtoDLet a name (toProtoLetDefinition def)
    DImport a path imps ->
      ProtoDImport a path imps
    DQualifiedImport a path ->
      ProtoDQualifiedImport a path
    DTrait a name def ->
      toProtoTraitDefinition a name def
    DInstance a name def ->
      toProtoInstanceDefinition a name def
    DTypeAlias a name def ->
      ProtoDTypeAlias a name (toProtoAliasDefinition def)
    DFold a name def ->
      ProtoDFold a name (toProtoFold a def)

toProtoModuleDefinitions :: [Definition a Kind t] -> [ProtoDefinition a () t]
toProtoModuleDefinitions = fmap toProtoModuleDefinition

fromProtoModuleDefinition :: ProtoDefinition a Kind t -> Definition a Kind t
fromProtoModuleDefinition =
  \case
    ProtoDType a name def ->
      DType a name (fromProtoTypeDefinition def)
    ProtoDTypeAlias a name def ->
      DTypeAlias a name (fromProtoAliasDefinition def)
    ProtoDFunction a name def ->
      DFunction a name (fromProtoFunction def :| []) []
    ProtoDFunctionGroup a name (def : defs) ->
      DFunction a name (fmap fromProtoFunction (def :| defs)) []
    ProtoDFunctionGroup a name [] ->
      error "???"
    ProtoDFold a name def ->
      DFold a name (fromProtoFold def)
    ProtoDLet a name def ->
      DConstant a name (fromProtoLetDefinition def) []
    ProtoDImport a path imps ->
      DImport a path imps
    ProtoDQualifiedImport a path ->
      DQualifiedImport a path
    ProtoDTrait a name def ->
      DTrait a name (fromProtoTraitDefinition def)
    ProtoDInstance a def ->
      fromProtoInstanceDefinition a def

toProtoModule :: [Definition a Kind ()] -> Module a Kind () -> ProtoModule a () ()
toProtoModule extra Module{..} =
  ProtoModule
    { protoOmodulePath = modulePath
    , protoOmoduleExportList = toProtoModuleExportList moduleExports
    , protoOmoduleDefinitions = toProtoModuleDefinitions (extra <> moduleDefinitions)
    }

fromProtoModule :: ProtoModule a Kind IndexedType -> Module a Kind IndexedType
fromProtoModule ProtoModule{..} =
  Module
    { modulePath = protoOmodulePath
    , moduleExports = fromProtoModuleExportList protoOmoduleExportList
    , moduleDefinitions = fmap fromProtoModuleDefinition protoOmoduleDefinitions
    }
