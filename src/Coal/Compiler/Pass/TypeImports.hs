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
import Extra (forM_)
import Text.Megaparsec (runParser)

typeImportsPass :: (MonadIO m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
typeImportsPass =
  Pass
    { passName = "TypeImports"
    , runPass = pass
    }

pass = undefined
