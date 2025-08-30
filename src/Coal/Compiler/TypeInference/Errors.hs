{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.TypeInference.Errors where

import Coal.Ast.Metadata
import Data.Text (Text)
import Text.Megaparsec
import TextShow (showt)

import qualified Data.Text as Text

prettyErrorMessage :: (HasMetadata a) => [Text] -> Text -> a -> Text
prettyErrorMessage msg src err =
  let spanLines = extractSpan src (locationStart meta) (locationEnd meta)
      locationLine = showt (unPos $ sourceLine (locationStart meta))
      locationCol = showt (unPos $ sourceColumn (locationStart meta))

      rendered =
        Text.unlines $
          ["  |"]
            <> concatMap
              ( \(_, line, marker) ->
                  [ "  | " <> line
                  , "  | " <> marker
                  ]
              )
              spanLines
            <> msg
   in locationLine
        <> ":"
        <> locationCol
        <> ":\n"
        <> rendered
 where
  meta = getMetadata err

extractSpan :: Text -> SourcePos -> SourcePos -> [(Int, Text, Text)]
extractSpan src start end =
  map extractPointer $
    filter (\(n, _) -> n >= startLine && n <= endLine) numberedLines
 where
  startLine = unPos (sourceLine start)
  endLine = unPos (sourceLine end)
  startCol = unPos (sourceColumn start)
  endCol = unPos (sourceColumn end)
  numberedLines = zip [1 ..] (Text.lines src)

  extractPointer (lineNum, lineText)
    | lineNum == startLine && startLine == endLine =
        let underline = Text.replicate (startCol - 1) " " <> Text.replicate (max 1 (endCol - startCol)) "^"
         in (lineNum, lineText, underline)
    | lineNum == startLine =
        let underline = Text.replicate (startCol - 1) " " <> Text.replicate (Text.length lineText - startCol + 1) "^"
         in (lineNum, lineText, underline)
    | lineNum > startLine && lineNum < endLine =
        let underline = Text.replicate (Text.length lineText) "^"
         in (lineNum, lineText, underline)
    | lineNum == endLine =
        let underline = Text.replicate (endCol - 1) "^"
         in (lineNum, lineText, underline)
    | otherwise = (lineNum, lineText, "")
