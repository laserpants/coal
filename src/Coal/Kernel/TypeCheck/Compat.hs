{- |
Type compatibility checking.

Provides the core 'compatible' function for determining whether two types are
mutually assignable.

= Compatibility rules

  * The opaque wildcard 'TOpq' (@*@) is compatible with any type in either
    direction.
  * Type constructors must have matching names and compatible arguments.
  * Row types are normalized (fields sorted by name) before comparison to
    ignore field ordering.
  * A field present in one row but not the other is acceptable when the other
    row's tail is 'TOpq' (meaning "any additional fields are fine").
-}
module Coal.Kernel.TypeCheck.Compat (
  compatible,
) where

import qualified Data.Map.Strict as Map

import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Row (toNormalForm)

{- | Check whether two types are mutually compatible.

'TOpq' (@*@) is treated as a wildcard: compatible with any type in either
direction. All other comparisons are structural. Row types are normalized
(fields sorted by name) before comparison so that field-ordering differences
do not produce false negatives.
-}
compatible :: Type -> Type -> Bool
compatible TOpq _ = True
compatible _ TOpq = True
compatible (TCon n1 ts1) (TCon n2 ts2) =
  n1 == n2
    && length ts1 == length ts2
    && all (uncurry compatible) (zip ts1 ts2)
compatible r1@(RExt{}) r2@(RExt{}) = compatibleRows r1 r2
compatible r1@(RExt{}) RNil = compatibleRows r1 RNil
compatible RNil r2@(RExt{}) = compatibleRows RNil r2
compatible RNil RNil = True
compatible _ _ = False

{- | Compare two row types after normalization.

Fields present in both sides must have compatible types. A field present in
only one side is acceptable when the other side's tail is 'TOpq' (meaning
\"any additional fields are fine\"). The two tails are checked recursively.
-}
compatibleRows :: Type -> Type -> Bool
compatibleRows r1 r2 =
  let (m1, tail1) = toNormalForm r1
      (m2, tail2) = toNormalForm r2
      -- Fields in both: pairwise compatible
      commonOk = and $ Map.intersectionWith compatible m1 m2
      -- Fields only in m1 are fine when m2's tail is opaque
      onlyIn1Ok = Map.null (Map.difference m1 m2) || isTOpq tail2
      -- Fields only in m2 are fine when m1's tail is opaque
      onlyIn2Ok = Map.null (Map.difference m2 m1) || isTOpq tail1
   in commonOk && onlyIn1Ok && onlyIn2Ok && compatible tail1 tail2

isTOpq :: Type -> Bool
isTOpq TOpq = True
isTOpq _ = False
