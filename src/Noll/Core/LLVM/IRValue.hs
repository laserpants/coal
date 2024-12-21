module Noll.Core.LLVM.IRValue (IRValue (..)) where

import Data.Int (Int32, Int64)
import Noll.Core.LLVM.IRType (IRType)
import Noll.Utils (Name)

data IRValue
  = Local IRType Name
  | Global IRType Name
  | I1 Bool
  | I32 Int32
  | I64 Int64
  | Float Float
  | Double Double
  | Null
  deriving (Show, Eq, Ord, Read)
