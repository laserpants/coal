{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IREncodable (
  IREncodable (..),
  IRAnnotated (..),
  IRLabel (..),
  irEncodeAnnotated,
  irEncodeLabel,
  irLocalName,
  irGlobalName,
  enquote,
) where

import Data.Char (isAlphaNum)
import Data.List (intersperse)
import Data.Text (Text)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Label (Label (..))
import Noll.Utils (Name)
import TextShow (showt)

import qualified Data.Text as Text

class IREncodable a where
  irEncode :: a -> Text

instance (IREncodable a) => IREncodable [a] where
  irEncode = Text.unlines . fmap irEncode

instance IREncodable Text where
  irEncode = id

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
        irEncode t <> " " <> ptr (parens (commaSep ts))

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

instance (IREncodable a) => IREncodable (IRConstruct a) where
  irEncode =
    \case
      CDefine name t ln as c ->
        -- TODO
        "define"
          <> " "
          <> global t name
          <> parens (commaSep as)
          <> " "
          <> funBlock c
          <> "\n"
      CDeclare name t ts ->
        "declare"
          <> " "
          <> global t name
          <> parens (commaSep ts)
          <> "\n"
      _ ->
        error "TODO"

enquote :: Text -> Text
enquote n
  | Text.all isAlphaNum n = n
  | otherwise = "\"" <> n <> "\""

funBlock :: (IREncodable a) => a -> Text
funBlock b = "{" <> "\n" <> irEncode b <> "}"

commaSep :: (IREncodable a) => [a] -> Text
commaSep = Text.concat . intersperse ", " . fmap irEncode

{-# INLINE irLocalName #-}
irLocalName :: Name -> Text
irLocalName n = "%" <> enquote n

{-# INLINE irGlobalName #-}
irGlobalName :: Name -> Text
irGlobalName n = "@" <> enquote n

{-# INLINE global #-}
global :: IRType -> Name -> Text
global t name = irEncode t <> " " <> irGlobalName name

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

{-# INLINE irEncodeAnnotated #-}
irEncodeAnnotated :: IRValue -> Text
irEncodeAnnotated = irEncode . IRAnnotated

{-# INLINE irEncodeLabel #-}
irEncodeLabel :: Text -> Text
irEncodeLabel = irEncode . IRLabel
