{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test12 where

-- Lowpass

-------------------

-- Utils

-- Ordered

{-

data Ordered.EqualTo<0,Ordered.Ordering>

data Ordered.GreaterThan<1,Ordered.Ordering>

data Ordered.LessThan<2,Ordered.Ordering>

Ordered.compare( a_1 : record({ compare : */*/Ordering | * })
               , a_2 : *
               , a_3 : *
               ) =
  match<Ordering>(a_1 : record({ compare : */*/Ordering | * })) {
    | ( $Record : { compare : */*/Ordering | * }/record({ compare : */*/Ordering | * })
      , r_1 : { compare : */*/Ordering | * }
      ) =>
        select
          { compare = f_1 : */*/Ordering | q_1 : * } =
            r_1 : { compare : */*/Ordering | * }
          in
            @<Ordering>(f_1 : */*/Ordering, a_2 : *, a_3 : *)
  }

// instance Ordered.Ordered(int32)

Ordered.Ordered_compare_instance_1(x : int32, y : int32) =
  if ([< int32](x : int32, y : int32))
    then Ordered.LessThan : Ordered.Ordering
    else
      if ([> int32](x : int32, y : int32))
        then Ordered.GreaterThan : Ordered.Ordering
        else Ordered.EqualTo : Ordered.Ordering

-}

-- BinarySearch

-- Main
