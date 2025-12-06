{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Compiler.Pass.ParsingPhase.Parsing (passParsing, embedded) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Config
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Path.Resolve
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Parser (ParserError, parseModule)
import Coal.Parser.Core (spaces)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (gets)
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import Extras (forM, forM_)
import Text.Megaparsec (eof, runParser)

passParsing :: (MonadIO m) => Pass Metadata m [FilePath] [Module Metadata Kind ()]
passParsing =
  Pass
    { passName = "Parsing"
    , runPass = pass
    }

embedded :: [(Text, B.ByteString)]
embedded =
  [
    ( "IO"
    , $(embedFile "lang/IO.coal")
    )
  ,
    ( "List"
    , $(embedFile "lang/List.coal")
    )
  ,
    ( "Nat"
    , $(embedFile "lang/Nat.coal")
    )
  ,
    ( "Number"
    , $(embedFile "lang/Number.coal")
    )
  ,
    ( "String"
    , $(embedFile "lang/String.coal")
    )
  ,
    ( "Char"
    , $(embedFile "lang/Char.coal")
    )
  ,
    ( "Option"
    , $(embedFile "lang/Option.coal")
    )
  ,
    ( "Coal.Combinators"
    , $(embedFile "lang/Coal/Combinators.coal")
    )
  ,
    ( "Coal.Monoid"
    , $(embedFile "lang/Coal/Monoid.coal")
    )
  ,
    ( "Coal.Functor"
    , $(embedFile "lang/Coal/Functor.coal")
    )
  ,
    ( "Coal.Applicative"
    , $(embedFile "lang/Coal/Applicative.coal")
    )
  ,
    ( "Coal.Monad"
    , $(embedFile "lang/Coal/Monad.coal")
    )
  ]

pass :: (MonadIO m) => [FilePath] -> CompilerT Metadata m [Module Metadata Kind ()]
pass files = do
  embeddedFiles <- traverse parseEmbedded embedded
  case partitionEithers embeddedFiles of
    ([], embeddedBundle) -> do
      results <- traverse parseFile files
      case partitionEithers results of
        ([], bundle) ->
          pure (embeddedBundle <> bundle)
        (es, _) -> do
          forM_ es (tellErrors . return)
          throwError ParserFailure
    (es, _) -> do
      -- TODO
      forM es $
        \(p, e) ->
          error ("Error in embedded module '" <> Text.unpack p <> "': " <> show e)

parseEmbedded :: (MonadIO m) => (Text, B.ByteString) -> CompilerT Metadata m (Either (Text, ParserError) (Module Metadata Kind ()))
parseEmbedded (p, src) =
  case runParser (parseModule <* eof) "" encodedSrc of
    Left err ->
      pure $ Left (p, err)
    Right module_ -> do
      setVerbatimSourceForC module_ encodedSrc
      pure $ Right module_
 where
  encodedSrc = E.decodeUtf8 src

parseFile :: (MonadIO m) => FilePath -> CompilerT Metadata m (Either (CompilerError Metadata) (Module Metadata Kind ()))
parseFile file = do
  CompilerConfig{..} <- gets compilerConfig
  res <- liftIO $ resolveModule configSourcePaths file
  case res of
    Right (fp, _, name) -> do
      src <- Text.pack <$> liftIO (readFile fp)
      case runParser (spaces *> parseModule <* eof) "" src of
        Left err ->
          pure $ Left (ParserError file err)
        Right module_@(Module path _ _) -> do
          if Text.unpack (principalPath path) == name
            then do
              setVerbatimSourceForC module_ src
              pure $ Right module_
            else pure $ Left (InvalidModuleName file (principalPath path))
    Left err -> do
      pure $ Left (BadFilename file err)
