{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.LegacyKernel.LLVM.IREncodable (
  IREncodable (..),
  IRAnnotated (..),
  IRLabel (..),
  commaSep,
  annotated,
  encodeLabel,
  irLocalName,
  irGlobalName,
  enquote,
) where

import Coal.Common.Label (Label (..))
import Coal.LegacyKernel.LLVM.IRConstruct (IRConstruct (..), IRLinkage (..))
import Coal.LegacyKernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.LegacyKernel.LLVM.IRType.Syntax (i64, i8)
import Coal.LegacyKernel.LLVM.IRValue (IRValue (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Word (Word8)
import Extras (Name, (||.))
import Extras.Data.Char (isUnderscore)
import qualified Numeric
import TextShow (showt)

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
        Text.pack (Numeric.showFFloat Nothing f "")
      Double d ->
        Text.pack (Numeric.showFFloat Nothing d "")
      Null ->
        "null"
      Array _ vs ->
        brackets (commaSepLines vs)
      Bitcast t1 t2 n t3 ->
        irEncode t1
          <> space
          <> "bitcast"
          <> parens (global t2 n <> " to " <> irEncode t3)

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
        space
      Just e ->
        irEncode (padded e)

instance (IREncodable a) => IREncodable (IRConstruct a) where
  irEncode =
    \case
      CDefine name t ln as c ->
        linebreak $
          "define"
            <> irEncode ln
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
            <> " = type"
            <> space
            <> irEncode t
      CString name str ln ->
        let len = ByteString.length str
         in linebreak $
              irGlobalName name
                <> " = "
                <> maybe "" (\l -> irEncode l <> " ") ln
                <> "constant"
                <> space
                <> irEncode (TStruct [i64, TArray (len + 1) i8])
                <> space
                <> braces
                  ( "i64 "
                      <> showt len
                      <> ", "
                      <> irEncode (TArray (len + 1) i8)
                      <> space
                      <> Text.concat ["c\"", escapeString str, "\\00\""]
                  )
      COldStyleString name str ln ->
        let len = ByteString.length str
         in linebreak $
              irGlobalName name
                <> " = "
                <> maybe "" (\l -> irEncode l <> " ") ln
                <> "constant"
                <> space
                <> irEncode (TArray (len + 1) i8)
                <> space
                <> Text.concat ["c\"", escapeString str, "\\00\""]
      CExternGlobal name t ->
        linebreak $
          irGlobalName name
            <> " = external constant"
            <> space
            <> irEncode t
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

escapeString :: ByteString -> Text
escapeString = ByteString.foldr ((<>) . escapeByte) Text.empty
 where
  escapeByte :: Word8 -> Text
  escapeByte b
    | b == 34 = "\\22" -- double quote (")
    | b == 92 = "\\5C" -- backslash (\)
    | b == 10 = "\\0A" -- newline
    | b == 9 = "\\09" -- tab
    | b >= 32 && b <= 126 = Text.singleton (toEnum (fromEnum b)) -- printable ASCII
    | otherwise = Text.pack $ "\\" ++ padHex (Numeric.showHex b "")

  padHex :: String -> String
  padHex h
    | length h == 1 = "0" ++ h -- Ensure two hex digits
    | otherwise = h

{-# INLINE space #-}
space :: Text
space = " "

{-# INLINE linebreak #-}
linebreak :: Text -> Text
linebreak txt = txt <> "\n"

enquote :: Text -> Text
enquote name
  | Text.all (isAlphaNum ||. isUnderscore) name = name
  | otherwise = "\"" <> name <> "\""

funBlock :: (IREncodable a) => a -> Text
funBlock block = linebreak "{" <> irEncode block <> "}"

{-# INLINE commaSep #-}
commaSep :: (IREncodable a) => [a] -> Text
commaSep = Text.intercalate ", " . fmap irEncode

commaSepLines :: (IREncodable a) => [a] -> Text
commaSepLines as = "\n" <> Text.intercalate ", \n" (fmap (("  " <>) . irEncode) as) <> "\n"

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
