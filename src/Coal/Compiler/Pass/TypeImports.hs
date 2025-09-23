{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypeImports where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.Parser (ParserError)
import Coal.Parser.Module (parseModule)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as Text
import Extra (concatMapM, forM_, isConstructor)
import Text.Megaparsec (runParser)

typeImportsPass :: (MonadIO m) => Pass a m [Module a Kind ()] [Module a Kind ()]
typeImportsPass =
  Pass
    { passName = "TypeImports"
    , runPass = pass
    }

pass :: (Monad m) => [Module a Kind ()] -> CompilerT a m [Module a Kind ()]
pass = traverse (overModuleDefinitionsM xxx)

xxx :: (Monad m) => [Definition a k ()] -> CompilerT a m [Definition a k ()]
xxx defs = do
  z <- traverse go defs
  pure (concat z <> defs)

go :: (Monad m) => Definition a k () -> CompilerT a m [Definition a k ()]
go =
  \case
    DImport _ (Path path) ns ->
      pure [t | t@(DType _ c _) <- ds, c `elem` constructors]
     where
      constructors = filter isConstructor ns
      ds = undefined -- fromMaybe mempty (Environment.lookup (Text.intercalate "." path) env)
    _ ->
      pure []
