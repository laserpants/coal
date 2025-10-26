{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.Parser.Op (op) where

import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Parser (Parser, optional, try, ($>), (<|>))
import Coal.Kernel.Parser.Symbol (brackets, pair, parens, symbol)
import Coal.Kernel.Parser.Type (type_)

data Op2Symbol
  = SymEq
  | SymNe
  | SymLte
  | SymGte
  | SymLt
  | SymGt
  | SymAdd
  | SymSub
  | SymMul
  | SymDiv
  | SymOr
  | SymAnd
  deriving (Show, Eq, Ord, Read)

op2Symbol :: Parser Op2Symbol
op2Symbol =
  (symbol "==" $> SymEq)
    <|> (symbol "!=" $> SymNe)
    <|> (symbol "<=" $> SymLte)
    <|> (symbol ">=" $> SymGte)
    <|> (symbol "<" $> SymLt)
    <|> (symbol ">" $> SymGt)
    <|> (symbol "+" $> SymAdd)
    <|> (symbol "-" $> SymSub)
    <|> (symbol "*" $> SymMul)
    <|> (symbol "/" $> SymDiv)
    <|> (symbol "||" $> SymOr)
    <|> (symbol "&&" $> SymAnd)

op2 :: Parser (a -> a -> Op a)
op2 = do
  s <- op2Symbol
  t <- optional type_
  case t of
    Just (TCon "int32" []) ->
      case s of
        SymEq -> pure OEqInt32
        SymNe -> pure ONeInt32
        SymLte -> pure OLteInt32
        SymGte -> pure OGteInt32
        SymLt -> pure OLtInt32
        SymGt -> pure OGtInt32
        SymAdd -> pure OAddInt32
        SymSub -> pure OSubInt32
        SymMul -> pure OMulInt32
        SymDiv -> pure ODivInt32
        _ -> fail "Invalid operator"
    Just (TCon "int64" []) ->
      case s of
        SymEq -> pure OEqInt64
        SymNe -> pure ONeInt64
        SymLte -> pure OLteInt64
        SymGte -> pure OGteInt64
        SymLt -> pure OLtInt64
        SymGt -> pure OGtInt64
        SymAdd -> pure OAddInt64
        SymSub -> pure OSubInt64
        SymMul -> pure OMulInt64
        SymDiv -> pure ODivInt64
        _ -> fail "Invalid operator"
    Just (TCon "float" []) ->
      case s of
        SymEq -> pure OEqFloat
        SymNe -> pure ONeFloat
        SymLte -> pure OLteFloat
        SymGte -> pure OGteFloat
        SymLt -> pure OLtFloat
        SymGt -> pure OGtFloat
        SymAdd -> pure OAddFloat
        SymSub -> pure OSubFloat
        SymMul -> pure OMulFloat
        SymDiv -> pure ODivFloat
        _ -> fail "Invalid operator"
    Just (TCon "double" []) ->
      case s of
        SymEq -> pure OEqDouble
        SymNe -> pure ONeDouble
        SymLte -> pure OLteDouble
        SymGte -> pure OGteDouble
        SymLt -> pure OLtDouble
        SymGt -> pure OGtDouble
        SymAdd -> pure OAddDouble
        SymSub -> pure OSubDouble
        SymMul -> pure OMulDouble
        SymDiv -> pure ODivDouble
        _ -> fail "Invalid operator"
    Just (TCon "bool" []) ->
      case s of
        SymEq -> pure OEqBool
        _ -> fail "Invalid operator"
    Just (TCon "char" []) ->
      case s of
        SymEq -> pure OEqInt32
        _ -> fail "Invalid operator"
    Nothing ->
      case s of
        SymOr -> pure OOr
        SymAnd -> pure OAnd
        _ -> fail "Invalid operator"
    _ ->
      fail "Invalid operator"

op1 :: Parser (a -> Op a)
op1 = symbol "!" $> ONot

op :: Parser a -> Parser (Op a)
op p =
  try (brackets op1 <*> parens p)
    <|> (uncurry <$> brackets op2 <*> pair p p)
