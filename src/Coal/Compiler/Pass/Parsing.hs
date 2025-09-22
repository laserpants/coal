{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.Parsing (parsingPass) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Parser (ParserError)
import Coal.Parser.Module (parseModule)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either (partitionEithers)
import Data.Text (Text)
import qualified Data.Text as Text
import Extra (forM_)
import Text.Megaparsec (runParser)

parsingPass :: (MonadIO m) => Pass Metadata m [FilePath] [ModuleBundle]
parsingPass =
  Pass
    { passName = "Parsing"
    , runPass = pass
    }

pass :: (MonadIO m) => [FilePath] -> CompilerT Metadata m [ModuleBundle]
pass files = do
  contents <- liftIO (traverse readFile files)
  results <- traverse (parseFile . Text.pack) contents
  case partitionEithers results of
    ([], bundle) ->
      pure bundle
    (es, _) -> do
      forM_ es (\e -> tellErrors [ParserError e])
      throwError ParserFailure

parseFile :: (Monad m) => Text -> CompilerT Metadata m (Either ParserError ModuleBundle)
parseFile src = do
  case runParser parseModule "" src of
    Left e ->
      pure $ Left e
    Right module_ -> do
      setVerbatimSourceForC module_ src
      pure $ Right (src, module_)
