{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Noll.Compiler2.Environment (buildEnvironments, buildAliasEnv) where

import Control.Monad.State (evalState)
import Lang.Common.Environment (Environment (..))
import Lang.Utils (Name, Dictionary, (<$$>))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Compiler2.Parameterized
import Noll.Language
import Noll.Module.Definition
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map
import qualified Lang.Common.Environment as Environment
import qualified Data.Text as Text

buildEnvironments :: [Definition a k t] -> (Environment Kind, Environment (Constructor TypeIndex Kind IndexedType), Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType)), AliasEnvironment)
buildEnvironments defs = (e1, e2, e3, e4)
 where
  e4 = buildAliasEnv defs
  e3 = buildTraitEnvironment e1 defs
  e2 = buildDataConstructorEnv e1 defs
  e1 = buildTypeConstructorEnv defs

buildDataConstructorEnv :: Environment Kind -> [Definition a k t] -> Environment (Constructor TypeIndex Kind IndexedType)
buildDataConstructorEnv env = Environment.fromList . concatMap go
 where
  go =
    \case
      DType _ _ cs ->
        translateConstructor <$> cs
      _ ->
        []
  translateConstructor (Constructor n a s) =
    (n, Constructor n a (translateScheme s))
  translateScheme (Forall _ _ t) =
    Forall (typeIndexesIn t1) [] t1
   where
    t1 = evalState (instantiateVars [] env t) (0 :: Int)

buildTypeConstructorEnv :: [Definition a k t] -> Environment Kind
buildTypeConstructorEnv = Environment.fromList . concatMap go
 where
  go =
    \case
      DType name ps _ ->
        [
          ( name
          , foldr KArrow KType (replicate (length ps) KType)
          )
        ]
      _ ->
        []

buildTraitEnvironment :: Environment Kind -> [Definition a k t] -> Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType))
buildTraitEnvironment env = Environment.fromList . concatMap go
 where
  go =
    \case
      DTrait name _ (Parameter k n) ds ->
        [
          ( name
          , (TypeIndex k 0, Environment.fromList (f <$$> ds))
          )
        ]
       where
        f t =
          let t1 = evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int)
           in Forall (typeIndexesIn t1) [] t1
      _ ->
        []

unions :: [(Name, Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))]
       -> Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
unions = foldr xx mempty

xx :: (Name, Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType))) 
    -> Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
    -> Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
xx = undefined

buildInstanceEnvironment :: 
  Environment (TypeIndex Kind, Environment (Scheme TypeIndex Kind IndexedType)) ->
  [Definition a k t] -> 
  Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
buildInstanceEnvironment env ds = unions (xnork <$> fazbox) -- Environment.fromList . concatMap go
 where
  fazbox = concatMap go ds
  go =
    \case
      DInstance name t ds ->
        case Environment.lookup name env of
          Nothing -> 
            error ("Trait '" <> Text.unpack name <> "' not in scope")
          Just (t1, env1) ->
            [ ( name, [ ( t1, gork <$> ds ) ] ) ]
      _ ->
        []

xnork :: (Name, [(TypeIndex Kind, [(Name, Scheme TypeIndex Kind IndexedType)])])
      -> (Name, Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
xnork = undefined

gork :: Definition a k t -> (Name, Scheme TypeIndex Kind IndexedType)
gork = undefined

--box :: 
--  TypeIndex Kind -> 
--  Environment (Scheme TypeIndex Kind IndexedType) -> 
--  [Definition a k t] -> 
--  Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType))
--box t1 env1 ds = undefined
--  where
--    faz = fox <$> ds
--
--fox :: Definition a k t -> (Name, Scheme TypeIndex Kind IndexedType)
--fox = undefined

buildAliasEnv :: [Definition a k t] -> AliasEnvironment
buildAliasEnv = Environment.fromList . concatMap go
 where
  go =
    \case
      DTypeAlias name ps t ->
        [
          ( name
          ,
            ( parameterName <$> ps
            , t
            )
          )
        ]
      _ ->
        []
