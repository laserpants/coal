{-# LANGUAGE StrictData #-}

module Coal.Compiler.Progress (
  ProgressRef,
  renderBar,
  writeStatus,
  writeStatusSimple,
) where

import Data.IORef (IORef, readIORef)
import System.IO (hFlush, hPutStr, stderr)

type ProgressRef = IORef (Int, Int)

writeStatus :: ProgressRef -> String -> IO ()
writeStatus ref msg = do
  (done, total) <- readIORef ref
  let pct = if total > 0 then (done * 100) `div` total else 0
      bar = renderBar done total 30
  hPutStr stderr $ "\r\ESC[2K" <> bar <> " " <> show pct <> "% " <> msg
  hFlush stderr

renderBar :: Int -> Int -> Int -> String
renderBar done total width
  | total <= 0 = "\ESC[90m" <> replicate width '━' <> "\ESC[0m"
  | otherwise =
      let filled = min width ((done * width) `div` total)
          empty_ = width - filled
       in "\ESC[38;2;57;255;20m" <> replicate filled '━' <> "\ESC[90m" <> replicate empty_ '━' <> "\ESC[0m"

writeStatusSimple :: String -> IO ()
writeStatusSimple msg = do
  hPutStr stderr $ "\r\ESC[2K" <> msg
  hFlush stderr
