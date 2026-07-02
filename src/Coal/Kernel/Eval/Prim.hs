{-# LANGUAGE LambdaCase #-}

{- |
Primitive operations evaluation.

Handles evaluation of:

  * Literal primitives (unit, bool, integers, floats, chars, strings)
  * Binary operators (arithmetic, comparison, logical)
  * Unary operators (boolean negation)

Operators expect their operands to already be evaluated to values. Type
checking ensures operators are applied to compatible types.
-}
module Coal.Kernel.Eval.Prim (
  evalPrim,
  evalOp,
) where

import Coal.Kernel.Eval.State (EvalError (..), EvalM, throwEval)
import Coal.Kernel.Eval.Value (Value (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))

-- ---------------------------------------------------------------------------
-- Literal primitives
-- ---------------------------------------------------------------------------

evalPrim :: Prim -> Value
evalPrim = \case
  PUnit -> VUnit
  PBool b -> VBool b
  PInt32 n -> VInt32 n
  PInt64 n -> VInt64 n
  PBignum n -> VBignum n
  PFloat f -> VFloat f
  PDouble d -> VDouble d
  PChar c -> VChar c
  PString bs -> VString bs

-- ---------------------------------------------------------------------------
-- Operators
-- Operand values are already evaluated before this function is called.
-- ---------------------------------------------------------------------------

evalOp :: Op Value -> EvalM Value
evalOp = \case
  -- Equality
  OEqInt32 (VInt32 a) (VInt32 b) -> pure (VBool (a == b))
  OEqInt64 (VInt64 a) (VInt64 b) -> pure (VBool (a == b))
  OEqFloat (VFloat a) (VFloat b) -> pure (VBool (a == b))
  OEqDouble (VDouble a) (VDouble b) -> pure (VBool (a == b))
  OEqChar (VChar a) (VChar b) -> pure (VBool (a == b))
  OEqBool (VBool a) (VBool b) -> pure (VBool (a == b))
  -- Inequality
  ONeInt32 (VInt32 a) (VInt32 b) -> pure (VBool (a /= b))
  ONeInt64 (VInt64 a) (VInt64 b) -> pure (VBool (a /= b))
  ONeFloat (VFloat a) (VFloat b) -> pure (VBool (a /= b))
  ONeDouble (VDouble a) (VDouble b) -> pure (VBool (a /= b))
  ONeChar (VChar a) (VChar b) -> pure (VBool (a /= b))
  ONeBool (VBool a) (VBool b) -> pure (VBool (a /= b))
  -- Less than
  OLtInt32 (VInt32 a) (VInt32 b) -> pure (VBool (a < b))
  OLtInt64 (VInt64 a) (VInt64 b) -> pure (VBool (a < b))
  OLtFloat (VFloat a) (VFloat b) -> pure (VBool (a < b))
  OLtDouble (VDouble a) (VDouble b) -> pure (VBool (a < b))
  -- Greater than
  OGtInt32 (VInt32 a) (VInt32 b) -> pure (VBool (a > b))
  OGtInt64 (VInt64 a) (VInt64 b) -> pure (VBool (a > b))
  OGtFloat (VFloat a) (VFloat b) -> pure (VBool (a > b))
  OGtDouble (VDouble a) (VDouble b) -> pure (VBool (a > b))
  -- Less than or equal
  OLteInt32 (VInt32 a) (VInt32 b) -> pure (VBool (a <= b))
  OLteInt64 (VInt64 a) (VInt64 b) -> pure (VBool (a <= b))
  OLteFloat (VFloat a) (VFloat b) -> pure (VBool (a <= b))
  OLteDouble (VDouble a) (VDouble b) -> pure (VBool (a <= b))
  -- Greater than or equal
  OGteInt32 (VInt32 a) (VInt32 b) -> pure (VBool (a >= b))
  OGteInt64 (VInt64 a) (VInt64 b) -> pure (VBool (a >= b))
  OGteFloat (VFloat a) (VFloat b) -> pure (VBool (a >= b))
  OGteDouble (VDouble a) (VDouble b) -> pure (VBool (a >= b))
  -- Addition
  OAddInt32 (VInt32 a) (VInt32 b) -> pure (VInt32 (a + b))
  OAddInt64 (VInt64 a) (VInt64 b) -> pure (VInt64 (a + b))
  OAddFloat (VFloat a) (VFloat b) -> pure (VFloat (a + b))
  OAddDouble (VDouble a) (VDouble b) -> pure (VDouble (a + b))
  -- Subtraction
  OSubInt32 (VInt32 a) (VInt32 b) -> pure (VInt32 (a - b))
  OSubInt64 (VInt64 a) (VInt64 b) -> pure (VInt64 (a - b))
  OSubFloat (VFloat a) (VFloat b) -> pure (VFloat (a - b))
  OSubDouble (VDouble a) (VDouble b) -> pure (VDouble (a - b))
  -- Multiplication
  OMulInt32 (VInt32 a) (VInt32 b) -> pure (VInt32 (a * b))
  OMulInt64 (VInt64 a) (VInt64 b) -> pure (VInt64 (a * b))
  OMulFloat (VFloat a) (VFloat b) -> pure (VFloat (a * b))
  OMulDouble (VDouble a) (VDouble b) -> pure (VDouble (a * b))
  -- Division
  ODivInt32 (VInt32 a) (VInt32 b) -> pure (VInt32 (a `div` b))
  ODivInt64 (VInt64 a) (VInt64 b) -> pure (VInt64 (a `div` b))
  ODivFloat (VFloat a) (VFloat b) -> pure (VFloat (a / b))
  ODivDouble (VDouble a) (VDouble b) -> pure (VDouble (a / b))
  -- Logical
  OAnd (VBool a) (VBool b) -> pure (VBool (a && b))
  OOr (VBool a) (VBool b) -> pure (VBool (a || b))
  ONot (VBool a) -> pure (VBool (not a))
  -- Negation
  ONegInt32 (VInt32 a) -> pure (VInt32 (negate a))
  ONegInt64 (VInt64 a) -> pure (VInt64 (negate a))
  ONegFloat (VFloat a) -> pure (VFloat (negate a))
  ONegDouble (VDouble a) -> pure (VDouble (negate a))
  -- Catch-all: operator applied to wrong value kind
  op -> throwEval (TypeMismatch (describeOp op) "unexpected value type for operand")

-- | Short description of an operator for error messages.
describeOp :: Op a -> String
describeOp = \case
  OEqInt32{} -> "[== int32]"
  OEqInt64{} -> "[== int64]"
  OEqFloat{} -> "[== float]"
  OEqDouble{} -> "[== double]"
  OEqChar{} -> "[== char]"
  OEqBool{} -> "[== bool]"
  ONeInt32{} -> "[!= int32]"
  ONeInt64{} -> "[!= int64]"
  ONeFloat{} -> "[!= float]"
  ONeDouble{} -> "[!= double]"
  ONeChar{} -> "[!= char]"
  ONeBool{} -> "[!= bool]"
  OLtInt32{} -> "[< int32]"
  OLtInt64{} -> "[< int64]"
  OLtFloat{} -> "[< float]"
  OLtDouble{} -> "[< double]"
  OGtInt32{} -> "[> int32]"
  OGtInt64{} -> "[> int64]"
  OGtFloat{} -> "[> float]"
  OGtDouble{} -> "[> double]"
  OLteInt32{} -> "[<= int32]"
  OLteInt64{} -> "[<= int64]"
  OLteFloat{} -> "[<= float]"
  OLteDouble{} -> "[<= double]"
  OGteInt32{} -> "[>= int32]"
  OGteInt64{} -> "[>= int64]"
  OGteFloat{} -> "[>= float]"
  OGteDouble{} -> "[>= double]"
  OAddInt32{} -> "[+ int32]"
  OAddInt64{} -> "[+ int64]"
  OAddFloat{} -> "[+ float]"
  OAddDouble{} -> "[+ double]"
  OSubInt32{} -> "[- int32]"
  OSubInt64{} -> "[- int64]"
  OSubFloat{} -> "[- float]"
  OSubDouble{} -> "[- double]"
  OMulInt32{} -> "[* int32]"
  OMulInt64{} -> "[* int64]"
  OMulFloat{} -> "[* float]"
  OMulDouble{} -> "[* double]"
  ODivInt32{} -> "[/ int32]"
  ODivInt64{} -> "[/ int64]"
  ODivFloat{} -> "[/ float]"
  ODivDouble{} -> "[/ double]"
  OAnd{} -> "[&&]"
  OOr{} -> "[||]"
  ONot{} -> "[!]"
  ONegInt32{} -> "[neg int32]"
  ONegInt64{} -> "[neg int64]"
  ONegFloat{} -> "[neg float]"
  ONegDouble{} -> "[neg double]"
