{-# LANGUAGE OverloadedStrings #-}

{- | Terminal capability detection and output sanitization.

This module detects whether the terminal supports UTF-8 and ANSI escape codes,
and provides functions to degrade output gracefully on unsupported terminals.
-}
module Coal.Compiler.Terminal (
  TerminalCapabilities (..),
  detectTerminalCapabilities,
  detectStderrCapabilities,
  sanitizeForTerminal,
  progressChar,
) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.Console.ANSI (hSupportsANSI)
import System.IO (Handle, hIsTerminalDevice, stderr)

data TerminalCapabilities = TerminalCapabilities
  { termSupportsUnicode :: Bool
  -- ^ Whether the terminal supports Unicode (UTF-8) output.
  , termSupportsANSI :: Bool
  -- ^ Whether the terminal supports ANSI escape codes.
  , termIsTerminal :: Bool
  -- ^ Whether the output handle is a terminal device (vs. a file/pipe).
  }
  deriving (Show, Eq)

{- | Detect terminal capabilities for the given handle.
Returns a default ASCII-only setup if detection fails.
-}
detectTerminalCapabilities :: Handle -> IO TerminalCapabilities
detectTerminalCapabilities h = do
  isTerm <- hIsTerminalDevice h
  supportsANSI <- hSupportsANSI h
  -- If the terminal supports ANSI, it almost certainly supports UTF-8.
  -- If detection fails, fall back to ASCII-only mode.
  let isUnicode = isTerm && supportsANSI
  pure $
    TerminalCapabilities
      { termSupportsUnicode = isUnicode
      , termSupportsANSI = isTerm && supportsANSI
      , termIsTerminal = isTerm
      }

-- | Detect terminal capabilities for stderr (used for progress and error output).
detectStderrCapabilities :: IO TerminalCapabilities
detectStderrCapabilities = detectTerminalCapabilities stderr

{- | Replace Unicode characters with ASCII alternatives when the terminal
doesn't support Unicode. No-op when 'termSupportsUnicode' is True.
-}
sanitizeForTerminal :: TerminalCapabilities -> Text -> Text
sanitizeForTerminal caps text
  | termSupportsUnicode caps = text
  | otherwise =
      Text.replace "\x2022" "*" -- • (BULLET)
        . Text.replace "\x2192" "->" -- → (RIGHTWARDS ARROW)
        $ text

{- | Choose the appropriate progress bar character based on terminal capabilities.
The 'Bool' parameter is 'True' for the filled portion, 'False' for the empty portion.
-}
progressChar :: TerminalCapabilities -> Bool -> Char
progressChar caps filled
  | termSupportsUnicode caps = if filled then '━' else '─'
  | otherwise = if filled then '=' else '-'
