{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.TypeInference.Errors (prettyErrorMessage) where

import Coal.Compiler.HasMetadata (HasMetadata (..))
import Coal.Compiler.Metadata (Metadata (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (SourcePos (sourceColumn, sourceLine), unPos)
import TextShow (showt)

prettyErrorMessage :: (HasMetadata a) => [Text] -> a -> Text -> Text
prettyErrorMessage msg err src =
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
  trimTrailingBlankLines $
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

  -- Trim trailing lines that are blank, whitespace-only, or where the error
  -- span only barely touches the beginning (e.g., the start of the next definition)
  trimTrailingBlankLines :: [(Int, Text, Text)] -> [(Int, Text, Text)]
  trimTrailingBlankLines = reverse . dropWhile isIrrelevantLine . reverse

  isIrrelevantLine :: (Int, Text, Text) -> Bool
  isIrrelevantLine (_, lineText, marker)
    -- Line is blank or whitespace-only
    | Text.null (Text.strip lineText) = True
    -- Line has content but the error marker only touches the very beginning
    -- (likely just capturing the start position of the next definition)
    | otherwise = isOnlyStartMarker marker
   where
    isOnlyStartMarker m =
      let stripped = Text.stripStart m
       in not (Text.null stripped) && Text.length stripped <= 2
