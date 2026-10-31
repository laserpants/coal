{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Functions (machineType, freeType, builtinFunctions) where

import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name)

machineType :: IndexedType -> IndexedType -> IndexedType
machineType i o = applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Machine") (i :| [o])

{- | @Free<f, a>@, the free monad over a functor @f@.

The type itself is declared in @Coal.Monad.Free@; its first parameter has kind
@Type -> Type@, so the type constructor's kind is
@(Type -> Type) -> Type -> Type@.
-}
freeType :: IndexedType -> IndexedType -> IndexedType
freeType f a =
  applyTypeArgs
    KType
    (TConstructor (KArrow (KArrow KType KType) (KArrow KType KType)) "Free")
    (f :| [a])

{- | Build the scheme of a Free operation.

All three operations are quantified over a functor @f@ (kind @Type -> Type@)
and two value types @a@ and @b@ (kind @Type@), and all of them carry a
@Functor<f>@ constraint: @Free<f, a>@ embeds @f<Free<f, a>>@, so reaching the
recursive occurrences requires mapping over the functor layer.

The three variables must have distinct indices: substitutions are keyed by
'typeIndexId' alone, so two quantified variables sharing an index would be
collapsed into one.
-}
freeScheme :: (IndexedType -> IndexedType -> IndexedType -> IndexedType) -> IndexedScheme
freeScheme body =
  Forall
    (Set.fromList [fVar, aVar, bVar])
    [Trait "Functor" (TVariable fVar)]
    (body (TVariable fVar) (TVariable aVar) (TVariable bVar))
 where
  fVar = TypeIndex (KArrow KType KType) 0
  aVar = TypeIndex KType 1
  bVar = TypeIndex KType 2

{- | @(f<m<a>> -> m<a>) -> Free<f, a> -> m<a>@, the type of the Free monad's
interpreter.

The handler consumes one layer of instructions whose continuations are already
@m@-computations; the program is then folded bottom-up.
-}
freeInterpretType :: IndexedType -> IndexedType -> IndexedType -> IndexedType
freeInterpretType f m a = (f `appliedTo` (m `appliedTo` a) ~> m `appliedTo` a) ~> freeType f a ~> m `appliedTo` a
 where
  appliedTo t u = applyTypeArgs KType t (u :| [])

{- | The scheme of the kernel-level Free interpreter
(@Builtin$.free$_interpret@).

Interpreting a program needs @Applicative\<m\>@ for the @pure@ of a finished
computation and @Functor\<f\>@ to map the interpreter under the instruction
layer. The trait list is also the dictionary parameter order of the kernel
definition: both dictionaries come first, in the order listed here, followed by
the handler and the program.

The three variables must have distinct indices; see 'freeScheme'.
-}
freeInterpretScheme :: IndexedScheme
freeInterpretScheme =
  Forall
    (Set.fromList [fVar, mVar, aVar])
    [Trait "Applicative" (TVariable mVar), Trait "Functor" (TVariable fVar)]
    (freeInterpretType (TVariable fVar) (TVariable mVar) (TVariable aVar))
 where
  fVar = TypeIndex (KArrow KType KType) 0
  mVar = TypeIndex (KArrow KType KType) 1
  aVar = TypeIndex KType 2

builtinFunctions :: [(Name, IndexedScheme)]
builtinFunctions =
  [
    ( "operator$__not"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    )
  ,
    ( "operator$__reverse_composition"
    , forall3 $ \t0 t1 t2 -> (t1 ~> t2) ~> (t0 ~> t1) ~> t0 ~> t2
    )
  ,
    ( "operator$__forward_composition"
    , forall3 $ \t0 t1 t2 -> (t0 ~> t1) ~> (t1 ~> t2) ~> t0 ~> t2
    )
  ,
    ( "operator$__reverse_application"
    , forall2 $ \t0 t1 -> t0 ~> (t0 ~> t1) ~> t1
    )
  ,
    ( "operator$__forward_application"
    , forall2 $ \t0 t1 -> (t0 ~> t1) ~> t0 ~> t1
    )
  ,
    ( "operator$__list_concatenation"
    , forall1 $ \t0 -> listType t0 ~> listType t0 ~> listType t0
    )
  ,
    ( "io$_println_int32"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_println_int64"
    , forall0 (TIntrinsic IInt64 ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_println_integer"
    , forall0 (TIntrinsic IBignum ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_println_string"
    , forall0 (TIntrinsic IString ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_println_bool"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_println_char"
    , forall0 (TIntrinsic IChar ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_println_float"
    , forall0 (TIntrinsic IFloat ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_println_double"
    , forall0 (TIntrinsic IDouble ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_int32"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_int64"
    , forall0 (TIntrinsic IInt64 ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_integer"
    , forall0 (TIntrinsic IBignum ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_string"
    , forall0 (TIntrinsic IString ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_bool"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_char"
    , forall0 (TIntrinsic IChar ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_float"
    , forall0 (TIntrinsic IFloat ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_print_double"
    , forall0 (TIntrinsic IDouble ~> TIntrinsic IUnit)
    )
  ,
    ( "io$_eval"
    , forall1 $ \t0 -> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (t0 :| []) ~> t0
    )
  ,
    ( "io$_return"
    , forall1 $ \t0 -> t0 ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (t0 :| [])
    )
  ,
    ( "io$_defer"
    , forall1 $ \t0 -> (TIntrinsic IUnit ~> t0) ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (t0 :| [])
    )
  ,
    ( "operator$__string_concatenation"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_char_to_string"
    , forall0 (TIntrinsic IChar ~> TIntrinsic IString)
    )
  ,
    ( "string$_bool_to_string"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IString)
    )
  ,
    ( "string$_int32_to_string"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic IString)
    )
  ,
    ( "string$_int64_to_string"
    , forall0 (TIntrinsic IInt64 ~> TIntrinsic IString)
    )
  ,
    ( "string$_float_to_string"
    , forall0 (TIntrinsic IFloat ~> TIntrinsic IString)
    )
  ,
    ( "string$_double_to_string"
    , forall0 (TIntrinsic IDouble ~> TIntrinsic IString)
    )
  ,
    ( "string$_to_list"
    , forall0 (TIntrinsic IString ~> listType (TIntrinsic IChar))
    )
  ,
    ( "string$_from_list"
    , forall0 (listType (TIntrinsic IChar) ~> TIntrinsic IString)
    )
  ,
    ( "string$_tail"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_reverse"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_remove_whitespace"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_head_unsafe"
    , forall0 (TIntrinsic IString ~> TIntrinsic IChar)
    )
  ,
    ( "string$_length"
    , forall0 (TIntrinsic IString ~> TIntrinsic IInt64)
    )
  ,
    ( "nat$_unpack"
    , forall0 (TIntrinsic INat ~> TIntrinsic IInt64)
    )
  ,
    ( "nat$_pack"
    , forall0 (TIntrinsic IInt64 ~> TIntrinsic INat)
    )
  ,
    ( "not"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    )
  ,
    ( "char$_ord"
    , forall0 (TIntrinsic IChar ~> TIntrinsic IInt32)
    )
  ,
    ( "char$_chr"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic IChar)
    )
  ,
    ( "number$_unsafe_parse_integer"
    , forall0 (TIntrinsic IString ~> TIntrinsic IBignum)
    )
  ,
    ( "from_int32"
    , forall1' (\t0 -> ([Trait "NumericBase" t0], TIntrinsic IInt32 ~> t0))
    )
  ,
    ( "from_int64"
    , forall1' (\t0 -> ([Trait "NumericBase" t0], TIntrinsic IInt64 ~> t0))
    )
  ,
    ( "from_integer"
    , forall1' (\t0 -> ([Trait "NumericBase" t0], TIntrinsic IBignum ~> t0))
    )
  ,
    ( "from_negative_int32"
    , forall1' (\t0 -> ([Trait "Numeric" t0], TIntrinsic IInt32 ~> t0))
    )
  ,
    ( "from_negative_int64"
    , forall1' (\t0 -> ([Trait "Numeric" t0], TIntrinsic IInt64 ~> t0))
    )
  ,
    ( "from_negative_integer"
    , forall1' (\t0 -> ([Trait "Numeric" t0], TIntrinsic IBignum ~> t0))
    )
  ,
    ( "negate"
    , forall1' (\t0 -> ([Trait "Numeric" t0], t0 ~> t0))
    )
  ,
    ( "compare"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TConstructor KType "Ordering"))
    )
  ,
    ( "(<)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(>)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(<=)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(>=)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(^)"
    , forall1' (\t0 -> ([Trait "NumericBase" t0], t0 ~> TIntrinsic INat ~> t0))
    )
  ,
    ( "(!=)"
    , forall1' (\t0 -> ([Trait "Comparable" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "machine$_map_machine"
    , forall3 $ \t1 t2 t3 -> (t2 ~> t3) ~> machineType t1 t2 ~> machineType t1 t3
    )
  ,
    ( "machine$_contramap_input"
    , forall3 $ \t1 t2 t3 -> (t3 ~> t1) ~> machineType t1 t2 ~> machineType t3 t2
    )
  ,
    ( "machine$_cofix"
    , forall2 $ \t1 t2 -> (machineType t1 t2 ~> machineType t1 t2) ~> machineType t1 t2
    )
  ,
    ( "machine$_run_while"
    , forall1 $ \t -> (TIntrinsic IUnit ~> TIntrinsic IBool) ~> machineType (TIntrinsic IUnit) t ~> t
    )
  ,
    ( "event$_blocking_poll"
    , forall2 $ \state result ->
        state
          ~> (state ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (applyTypeArgs KType (TConstructor (KArrow KType KType) "Option") (result :| []) :| []))
          ~> (state ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
          ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (result :| [])
    )
  ,
    ( "event$_loop"
    , forall1 $ \state ->
        state
          ~> (state ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "Option") (state :| []))
          ~> state
    )
  ,
    ( "event$_loop_io"
    , forall1 $ \state ->
        state
          ~> (state ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (applyTypeArgs KType (TConstructor (KArrow KType KType) "Option") (state :| []) :| []))
          ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (state :| [])
    )
  ,
    ( "runtime$_panic"
    , forall1 $ \t1 -> TIntrinsic IString ~> t1
    )
  ,
    ( "free$_map_free"
    , freeScheme $ \f a b -> (a ~> b) ~> freeType f a ~> freeType f b
    )
  ,
    ( "free$_ap_free"
    , freeScheme $ \f a b -> freeType f (a ~> b) ~> freeType f a ~> freeType f b
    )
  ,
    ( "free$_bind_free"
    , freeScheme $ \f a b -> freeType f a ~> (a ~> freeType f b) ~> freeType f b
    )
  ,
    ( "free$_interpret"
    , freeInterpretScheme
    )
  ]
