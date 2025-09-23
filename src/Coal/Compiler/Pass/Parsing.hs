{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.Parsing (parsingPass) where

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
import Extra (forM_)
import Text.Megaparsec (runParser)

parsingPass :: (MonadIO m) => Pass Metadata m [FilePath] [Module Metadata Kind ()]
parsingPass =
  Pass
    { passName = "Parsing"
    , runPass = pass
    }

pass :: (MonadIO m) => [FilePath] -> CompilerT Metadata m [Module Metadata Kind ()]
pass files = do
  contents <- liftIO (traverse readFile files)
  results <- traverse (parseFile . Text.pack) contents
  case partitionEithers results of
    ([], bundle) ->
      pure bundle
    (es, _) -> do
      forM_ es (\e -> tellErrors [ParserError e])
      throwError ParserFailure

parseFile :: (Monad m) => Text -> CompilerT Metadata m (Either ParserError (Module Metadata Kind ()))
parseFile src = setSource >> pure r
 where
  r = runParser parseModule "" src
  setSource =
    case r of
      Left{} ->
        pure ()
      Right module_ -> do
        setVerbatimSourceForC module_ src
