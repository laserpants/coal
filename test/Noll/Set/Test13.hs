{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test13 where

-- Lowpass

-------------------

-- Utils

-------------------

-- Ordered

{-

data Ordered.EqualTo<0, Ordered.Ordering>

data Ordered.GreaterThan<1, Ordered.Ordering>

data Ordered.LessThan<2, Ordered.Ordering>

Ordered.compare(a_1 : record({ compare : */*/Ordered.Ordering | * }), a_2 : *, a_3 : *) =
  match<Ordered.Ordering>(a_1 : record({ compare : */*/Ordered.Ordering | * }))
  { | ( $Record : { compare : */*/Ordered.Ordering | * }/record({ compare : */*/Ordered.Ordering | * })
      , r_1 : { compare : */*/Ordered.Ordering | * }
      ) =>
        select
          { compare = f_1 : */*/Ordered.Ordering | q_1 : * } =
            r_1 : { compare : */*/Ordered.Ordering | * }
          in
            @<Ordered.Ordering>(f_1 : */*/Ordered.Ordering, a_2 : *, a_3 : *)
  }

// instance Ordered.Ordered(int32)

Ordered.Ordered_compare_instance_1(x : int32, y : int32) =
  if ([< int32](x : int32, y : int32))
    then Ordered.LessThan : Ordered.Ordering
    else
      if ([> int32](x : int32, y : int32))
        then Ordered.GreaterThan : Ordered.Ordering
        else Ordered.EqualTo : Ordered.Ordering

Ordered.less_than_or_equal_to(d_1 : Ordered.Ordered(*), m : *, n : *) =
  match<bool>
    ( @<Ordered.Ordering>
      ( Ordered.compare : Ordered.Ordered(*)/*/*/Ordered.Ordering
      , d_1 : Ordered.Ordered(*)
      , m : *
      , n : *
      ))
  { | (EqualTo : Ordered.Ordering) => true
    | (GreaterThan : Ordered.Ordering) => false
    | (LessThan : Ordered.Ordering) => true
  }

Ordered.greater_than(d_1 : Ordered.Ordered(*), n : *) =
  [<<]
    ( Utils.not : bool/bool
    , @<*/bool>
        ( Ordered.less_than_or_equal_to : Ordered.Ordered(*)/*/*/bool
        , d_1 : Ordered.Ordered(*)
        , n : *)
    )

-}

-------------------

-- BinarySearch

-------------------

-- Main
