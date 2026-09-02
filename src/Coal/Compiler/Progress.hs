{-# LANGUAGE StrictData #-}

module Coal.Compiler.Progress (
  ProgressRef,
  renderBar,
  writeStatus,
  writeStatusSimple,
  writeStatusSimpleUnsafe,
) where

import Coal.Compiler.Terminal (TerminalCapabilities (..), progressChar)
import Data.IORef (IORef, readIORef)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)

type ProgressRef = IORef (Int, Int)

writeStatus :: TerminalCapabilities -> ProgressRef -> String -> IO ()
writeStatus caps ref msg
  | not (termIsTerminal caps) = hPutStrLn stderr msg
  | otherwise = do
      (done, total) <- readIORef ref
      let pct = if total > 0 then (done * 100) `div` total else 0
          bar = renderBar caps done total 30
      hPutStr stderr $ "\r\ESC[2K" <> bar <> " " <> show pct <> "% " <> msg
      hFlush stderr

renderBar :: TerminalCapabilities -> Int -> Int -> Int -> String
renderBar caps done total width
  | total <= 0 =
      let ch = progressChar caps True
       in withANSIStyle caps "\ESC[90m" (replicate width ch) <> "\ESC[0m"
  | otherwise =
      let filled = min width ((done * width) `div` total)
          empty_ = width - filled
          chFilled = progressChar caps True
          chEmpty = progressChar caps False
       in withANSIStyle caps "\ESC[38;2;57;255;20m" (replicate filled chFilled)
            <> withANSIStyle caps "\ESC[90m" (replicate empty_ chEmpty)
            <> "\ESC[0m"

writeStatusSimple :: TerminalCapabilities -> String -> IO ()
writeStatusSimple caps msg
  | not (termIsTerminal caps) = hPutStrLn stderr msg
  | otherwise = do
      hPutStr stderr $ "\r\ESC[2K" <> msg
      hFlush stderr

-- | Apply an ANSI style escape code only if the terminal supports ANSI.
withANSIStyle :: TerminalCapabilities -> String -> String -> String
withANSIStyle caps code str
  | termSupportsANSI caps = code <> str
  | otherwise = str

{- | Version of 'writeStatusSimple' that writes unconditionally without
terminal capability checks. Intended for internal use (e.g. @pipeline@
used by the test suite) where terminal detection is not relevant.
-}
writeStatusSimpleUnsafe :: String -> IO ()
writeStatusSimpleUnsafe msg = do
  hPutStr stderr $ "\r\ESC[2K" <> msg
  hFlush stderr
