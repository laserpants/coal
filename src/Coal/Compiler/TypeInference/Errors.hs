{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.TypeInference.Errors (prettyErrorMessage) where

import Coal.Compiler.HasMetadata (HasMetadata (..))
import Coal.Compiler.Metadata (Metadata (..))
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (SourcePos (sourceColumn, sourceLine), mkPos, unPos)
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
extractSpan src start end0 =
  trimTrailingBlankLines
    ( extractPointer
        <$> filter (\(n, _) -> n >= startLine && n <= endLine) numberedLines
    )
 where
  -- The parser records the end of a span after its last token's lexeme has
  -- already swallowed all following whitespace and comments, so the raw end
  -- usually sits just before the next construct. Recover the true end first.
  end = shrinkSpanEnd src start end0
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
  trimTrailingBlankLines lines_ =
    let trimmed = reverse . dropWhile isIrrelevantLine . reverse $ lines_
     in if null trimmed then lines_ else trimmed

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

{- | Shrink the recorded end of a span back to the last real character of the
construct it belongs to.

The parser consumes whitespace and comments /after/ every token, so a
recorded end position usually sits right before the next construct —
across blank lines, comments, or even the beginning of the following
definition. Everything between the true end of a construct and its
recorded end is exactly what the lexer's space consumer accepts:
whitespace, @\/\/@ line comments and matched @\/* .. *\/@ block comments.
Walking the source backwards through those yields the precise end.

Positions are mapped through flat character indices assuming LF line
endings, matching how @'extractSpan'@ slices the source.
-}
shrinkSpanEnd :: Text -> SourcePos -> SourcePos -> SourcePos
shrinkSpanEnd src start end
  | endIdx <= startIdx = end
  | otherwise = indexToPos src (max startIdx (Text.length stripped)) end
 where
  startIdx = posToIndex src start
  endIdx = posToIndex src end
  stripped = stripSpace (Text.take endIdx src)

  -- Remove trailing whitespace and comments from a chunk of source.
  stripSpace t
    | Text.null t = t
    | isSpace c = stripSpace (Text.init t)
    -- Trailing block comment: jump back to its opening @\/*@.
    | Text.isSuffixOf "*/" t
    , (block, _) <- Text.breakOnEnd "/*" t
    , not (Text.null block) =
        stripSpace (Text.dropEnd 2 block)
    -- Trailing line comment: cut everything from the last @\/\/@ onwards,
    -- keeping whatever precedes it on the same line for the next round.
    | (beforeComment, _) <- Text.breakOnEnd "//" (lastLine t)
    , not (Text.null beforeComment) =
        stripSpace
          ( Text.dropEnd (Text.length (lastLine t)) t
              <> Text.dropEnd 2 beforeComment
          )
    | otherwise = t
   where
    c = Text.last t

  -- The portion of @t@ after the last newline (the current line).
  lastLine u = snd (Text.breakOnEnd "\n" u)

-- | Convert a 1-based 'SourcePos' into a 0-based character index into @src@.
posToIndex :: Text -> SourcePos -> Int
posToIndex src pos =
  min
    (Text.length src)
    ( lineOffset (unPos (sourceLine pos) - 1) 0 (Text.lines src)
        + unPos (sourceColumn pos)
        - 1
    )
 where
  lineOffset n acc ls
    | n <= 0 = acc
    | otherwise = case ls of
        [] -> Text.length src
        (l : rest) -> lineOffset (n - 1) (acc + Text.length l + 1) rest

{- | Build a 'SourcePos' from a 0-based character index into @src@, keeping
the source name from the template position.
-}
indexToPos :: Text -> Int -> SourcePos -> SourcePos
indexToPos src idx template =
  template
    { sourceLine = mkPos lineNo
    , sourceColumn = mkPos colNo
    }
 where
  clamped = max 0 (min idx (Text.length src))
  parts = Text.splitOn "\n" (Text.take clamped src)
  lineNo = length parts
  colNo = Text.length (last parts) + 1
