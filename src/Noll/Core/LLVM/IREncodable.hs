{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREncodable (
  IREncodable (..),
  IRAnnotated (..),
  IRLabel (..),
  annotated,
  encodeLabel,
  irLocalName,
  irGlobalName,
  enquote,
) where

import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, ord)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..), IRLinkage (..))
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax (i8, i8Ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Label (Label (..))
import Noll.Utils (Name)
import TextShow (showt)

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text

class IREncodable a where
  irEncode :: a -> Text

instance (IREncodable a) => IREncodable [a] where
  irEncode = Text.unlines . fmap irEncode

instance IREncodable Text where
  irEncode = id

instance IREncodable ByteString where
  irEncode = decodeUtf8Lenient

instance IREncodable Int where
  irEncode = showt

instance IREncodable IRValue where
  irEncode =
    \case
      Local _ name ->
        irLocalName name
      Global _ name ->
        irGlobalName name
      I1 False ->
        "0"
      I1 True ->
        "1"
      I32 n ->
        showt n
      I64 n ->
        showt n
      Float f ->
        showt f
      Double d ->
        showt d
      Null ->
        "null"

instance IREncodable IRType where
  irEncode =
    \case
      TInt1 ->
        "i1"
      TInt8 ->
        "i8"
      TInt32 ->
        "i32"
      TInt64 ->
        "i64"
      TFloat ->
        "float"
      TDouble ->
        "double"
      TVoid ->
        "void"
      TNamed n _ ->
        irLocalName n
      TPtr t ->
        ptr t
      TStruct ts ->
        braces (padded (commaSep ts))
      TArray n t ->
        brackets (irEncode n <> " x " <> irEncode t)
      TFun t ts ->
        irEncode t <> space <> ptr (parens (commaSep ts))

newtype IRAnnotated v = IRAnnotated v
  deriving (Show, Eq, Ord, Read)

instance IREncodable (IRAnnotated IRValue) where
  irEncode (IRAnnotated v) = irEncode (irTypeOf v) <> " " <> irEncode v

newtype IRLabel = IRLabel Name
  deriving (Show, Eq, Ord, Read)

instance IREncodable IRLabel where
  irEncode (IRLabel name) = "label " <> irLocalName name

instance IREncodable (Label IRType) where
  irEncode (Label t name) =
    irEncode (IRAnnotated (Local t name))

instance IREncodable IRLinkage where
  irEncode =
    \case
      LInternal ->
        "internal"
      LPrivate ->
        "private"

instance (IREncodable e) => IREncodable (Maybe e) where
  irEncode =
    \case
      Nothing ->
        ""
      Just e ->
        irEncode e <> " "

instance (IREncodable a) => IREncodable (IRConstruct a) where
  irEncode =
    \case
      CDefine name t ln as c ->
        linebreak $
          "define"
            <> irEncode ln
            <> space
            <> global t name
            <> parens (commaSep as)
            <> space
            <> funBlock c
      CDeclare name t ts ->
        linebreak $
          "declare"
            <> space
            <> global t name
            <> parens (commaSep ts)
      CType name t ->
        linebreak $
          irLocalName name
            <> " = type "
            <> irEncode t
      CString name str ->
        linebreak $
          irGlobalName name
            <> " = private constant "
            <> irEncode (TArray (ByteString.length str + 1) i8)
            <> space
            <> Text.concat ["c\"", escapeString (decodeUtf8Lenient str), "\\00\""]
      CGlobal name t ln v ->
        linebreak $
          irGlobalName name
            <> " = "
            <> irEncode ln
            <> "global"
            <> space
            <> irEncode t
            <> space
            <> irEncode v

escapeString :: Text -> Text
escapeString = Text.concatMap escapeChar
 where
  escapeChar :: Char -> Text
  escapeChar '"' = "\\\"" -- Escape double quotes
  escapeChar '\\' = "\\\\" -- Escape backslashes
  escapeChar '\n' = "\\0A" -- LLVM-style newline escape
  escapeChar '\t' = "\\09" -- LLVM-style tab escape
  escapeChar c
    | c < ' ' || c > '~' = Text.pack (escapeUnicode c) -- Escape non-printables
    | otherwise = Text.singleton c

  escapeUnicode :: Char -> String
  escapeUnicode c = "\\x" ++ hex (ord c)

  hex :: Int -> String
  hex n = let h = "0123456789ABCDEF" in [h !! (n `div` 16), h !! (n `mod` 16)]

{-# INLINE space #-}
space :: Text
space = " "

{-# INLINE linebreak #-}
linebreak :: Text -> Text
linebreak txt = txt <> "\n"

enquote :: Text -> Text
enquote name
  | Text.all isAlphaNum name = name
  | otherwise = "\"" <> name <> "\""

funBlock :: (IREncodable a) => a -> Text
funBlock block = linebreak "{" <> irEncode block <> "}"

commaSep :: (IREncodable a) => [a] -> Text
commaSep = Text.intercalate ", " . fmap irEncode

{-# INLINE irLocalName #-}
irLocalName :: Name -> Text
irLocalName n = "%" <> enquote n

{-# INLINE irGlobalName #-}
irGlobalName :: Name -> Text
irGlobalName n = "@" <> enquote n

{-# INLINE global #-}
global :: IRType -> Name -> Text
global t name = irEncode t <> space <> irGlobalName name

{-# INLINE ptr #-}
ptr :: (IREncodable a) => a -> Text
ptr t = irEncode t <> "*"

{-# INLINE braces #-}
braces :: (IREncodable a) => a -> Text
braces t = "{" <> irEncode t <> "}"

{-# INLINE parens #-}
parens :: (IREncodable a) => a -> Text
parens t = "(" <> irEncode t <> ")"

{-# INLINE brackets #-}
brackets :: (IREncodable a) => a -> Text
brackets t = "[" <> irEncode t <> "]"

{-# INLINE padded #-}
padded :: (IREncodable a) => a -> Text
padded t = " " <> irEncode t <> " "

{-# INLINE annotated #-}
annotated :: IRValue -> Text
annotated = irEncode . IRAnnotated

{-# INLINE encodeLabel #-}
encodeLabel :: Text -> Text
encodeLabel = irEncode . IRLabel
