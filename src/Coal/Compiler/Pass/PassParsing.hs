{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PassParsing (parsingPass) where

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

parsingPass :: (MonadIO m) => Pass a m [FilePath] [ModuleBundle]
parsingPass =
  Pass
    { passName = "Parsing"
    , runPass = pass
    }

pass :: (MonadIO m) => [FilePath] -> CompilerT a m [ModuleBundle]
pass files = do
  contents <- liftIO (traverse readFile files)
  let results = fmap (parseFile . Text.pack) contents
  case partitionEithers results of
    ([], bundle) ->
      pure bundle
    (es, _) -> do
      forM_ es (\e -> tellErrors [ParserError e])
      throwError ParserFailure

parseFile :: Text -> Either ParserError ModuleBundle
parseFile src = do
  m <- runParser parseModule "" src
  pure (src, m)
