{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing) where

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
import qualified Data.Text as Text
import Extra (forM_)
import Text.Megaparsec (runParser)

passParsing :: (MonadIO m) => Pass Metadata m [FilePath] [Module Metadata Kind ()]
passParsing =
  Pass
    { passName = "Parsing"
    , runPass = pass
    }

pass :: (MonadIO m) => [FilePath] -> CompilerT Metadata m [Module Metadata Kind ()]
pass files = do
  results <- traverse parseFile files
  case partitionEithers results of
    ([], bundle) ->
      pure bundle
    (es, _) -> do
      forM_ es (\(file, e) -> tellErrors [ParserError file e])
      throwError ParserFailure

parseFile :: (MonadIO m) => FilePath -> CompilerT Metadata m (Either (FilePath, ParserError) (Module Metadata Kind ()))
parseFile file = do
  src <- Text.pack <$> liftIO (readFile file)
  case runParser parseModule "" src of
    Left err ->
      pure $ Left (file, err)
    Right module_ -> do
      setVerbatimSourceForC module_ src
      pure $ Right module_
