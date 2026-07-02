{- |
Module parsing utilities.

Provides convenience functions for parsing Coal kernel language modules from
text or files. Wraps the parser from "Coal.Kernel.Parser.Module" with I/O
handling.
-}
module Coal.Kernel.Eval.Load (
  parseModuleText,
  parseModuleFile,
) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Void (Void)

import Text.Megaparsec (ParseErrorBundle, parse)

import Coal.Kernel.Language.Module (Module)
import Coal.Kernel.Language.Type (Type)
import qualified Coal.Kernel.Parser.Module as Parser

{- | Parse a module from a 'Text' value.
The source name is used only for error messages.
-}
parseModuleText ::
  -- | Source name (e.g. file path or "<string>"), used in parse error messages.
  String ->
  Text ->
  Either (ParseErrorBundle Text Void) (Module Type)
parseModuleText = parse Parser.module_

-- | Read a file and parse it as a kernel language module.
parseModuleFile ::
  FilePath ->
  IO (Either (ParseErrorBundle Text Void) (Module Type))
parseModuleFile path = do
  src <- Text.readFile path
  return (parseModuleText path src)
