{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

{- |
Primitive operators.

Defines the full set of built-in binary operators for arithmetic, comparison,
and logical operations. Each operator is specialized to a concrete primitive
type.

= Operator categories

  * __Arithmetic__: @+@, @-@, @*@, @/@, @%@ for numeric types
  * __Comparison__: @==@, @!=@, @<@, @>@, @<=@, @>=@ for comparable types
  * __Logical__: @!@ for boolean negation

All operators are represented as constructors in the 'Op' type, parameterized
over their operand type @a@.
-}
module Coal.Kernel.Language.Op (Op (..)) where

{- | Primitive operators.

Each constructor represents a type-specific binary operation. Operands are
stored directly in the constructor, allowing 'Op' to be embedded in the
expression AST.
-}
data Op a
  = -- | Equality
    OEqInt32 a a
  | OEqInt64 a a
  | OEqFloat a a
  | OEqDouble a a
  | OEqChar a a
  | OEqBool a a
  | -- | Inequality
    ONeInt32 a a
  | ONeInt64 a a
  | ONeFloat a a
  | ONeDouble a a
  | ONeChar a a
  | ONeBool a a
  | -- | Less than
    OLtInt32 a a
  | OLtInt64 a a
  | OLtFloat a a
  | OLtDouble a a
  | -- | Greater than
    OGtInt32 a a
  | OGtInt64 a a
  | OGtFloat a a
  | OGtDouble a a
  | -- | Less than or equal to
    OLteInt32 a a
  | OLteInt64 a a
  | OLteFloat a a
  | OLteDouble a a
  | -- | Greater than or equal to
    OGteInt32 a a
  | OGteInt64 a a
  | OGteFloat a a
  | OGteDouble a a
  | -- | Addition
    OAddInt32 a a
  | OAddInt64 a a
  | OAddFloat a a
  | OAddDouble a a
  | -- | Subtraction
    OSubInt32 a a
  | OSubInt64 a a
  | OSubFloat a a
  | OSubDouble a a
  | -- | Multiplication
    OMulInt32 a a
  | OMulInt64 a a
  | OMulFloat a a
  | OMulDouble a a
  | -- | Division
    ODivInt32 a a
  | ODivInt64 a a
  | ODivFloat a a
  | ODivDouble a a
  | -- | Logical OR
    OOr a a
  | -- | Logical AND
    OAnd a a
  | -- | Logical NOT
    ONot a
  | -- | Negation
    ONegInt32 a
  | ONegInt64 a
  | ONegFloat a
  | ONegDouble a
  deriving
    ( Show
    , Eq
    , Ord
    , Read
    , Functor
    , Foldable
    , Traversable
    )
