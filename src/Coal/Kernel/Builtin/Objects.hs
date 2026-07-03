{-# LANGUAGE OverloadedStrings #-}

-- Scaffolded from Coal.LegacyKernel.Builtin.Objects.
-- Function bodies are stubs (undefined); fill them in with new-kernel Expr values.
module Coal.Kernel.Builtin.Objects (builtinObjects, builtinInstance) where

import qualified Coal.Compiler.Builtin.Traits as Trait
import qualified Coal.Kernel.Language.Expr as NK
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import qualified Coal.Kernel.Language.Type as NKT
import qualified Coal.Kernel.Language.Type.Constructors as NKC
import Coal.Language (
  Intrinsic (..),
  Parameter (..),
  Serializable,
  Trait (..),
  Type (TApplication, TConstructor, TIntrinsic, TVariable),
  instanceLabel,
 )
import Extras (Name)

builtinObjects :: Module NKT.Type
builtinObjects = objects

builtinInstance :: (Serializable t) => Trait t -> Name -> Name
builtinInstance trait name = instanceLabel trait ("Builtin$." <> name)

objects :: Module NKT.Type
objects =
  Module
    { moduleName = "Builtin$"
    , moduleImports = []
    , moduleObjects = objectList
    }

objectList :: [Object NKT.Type]
objectList =
  [ DData
      "Ordering"
      [ ("EqualTo", NKT.TCon "Ordering" [])
      , ("GreaterThan", NKT.TCon "Ordering" [])
      , ("LessThan", NKT.TCon "Ordering" [])
      ]
  , DFunction
      "Builtin$.operator$__not"
      [ NK.Label NKC.bool "a"
      ]
      undefined
  , DFunction
      "Builtin$.not"
      [ NK.Label NKC.bool "a"
      ]
      undefined
  , DFunction
      "Builtin$.char$_ord"
      [ NK.Label NKC.char "c"
      ]
      undefined
  , DFunction
      "Builtin$.char$_chr"
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      "Builtin$.number$_unsafe_parse_bignum"
      [ NK.Label NKC.string "input"
      ]
      undefined
  , DFunction
      "Builtin$.operator$__reverse_composition"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "g"
      , NK.Label NKT.TOpq "x"
      ]
      undefined
  , DFunction
      "Builtin$.operator$__forward_composition"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "g"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label NKT.TOpq "x"
      ]
      undefined
  , DFunction
      "Builtin$.operator$__reverse_application"
      [ NK.Label NKT.TOpq "x"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      ]
      undefined
  , DFunction
      "Builtin$.operator$__forward_application"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label NKT.TOpq "x"
      ]
      undefined
  , DFunction
      "Builtin$.always"
      [ NK.Label NKT.TOpq "a"
      , NK.Label NKT.TOpq "_"
      ]
      undefined
  , DFunction
      "Builtin$.operator$__list_concatenation"
      [ NK.Label (NKT.TCon "list" [NKT.TOpq]) "xs"
      , NK.Label (NKT.TCon "list" [NKT.TOpq]) "ys"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_int32"
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_int64"
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_bignum"
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_string"
      [ NK.Label NKC.string "s"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_bool"
      [ NK.Label NKC.bool "b"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_char"
      [ NK.Label NKC.char "c"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_float"
      [ NK.Label NKC.float "f"
      ]
      undefined
  , DFunction
      "Builtin$.io$_print_double"
      [ NK.Label NKC.double "d"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_int32"
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_int64"
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_bignum"
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_string"
      [ NK.Label NKC.string "s"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_bool"
      [ NK.Label NKC.bool "b"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_char"
      [ NK.Label NKC.char "c"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_float"
      [ NK.Label NKC.float "f"
      ]
      undefined
  , DFunction
      "Builtin$.io$_println_double"
      [ NK.Label NKC.double "d"
      ]
      undefined
  , DFunction
      "Builtin$.operator$__string_concatenation"
      [ NK.Label NKC.string "s"
      , NK.Label NKC.string "t"
      ]
      undefined
  , DFunction
      "Builtin$.string$_int32_to_string"
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      "Builtin$.string$_float_to_string"
      [ NK.Label NKC.float "f"
      ]
      undefined
  , DFunction
      "Builtin$.string$_double_to_string"
      [ NK.Label NKC.double "d"
      ]
      undefined
  , DFunction
      "Builtin$.string$_char_to_string"
      [ NK.Label NKC.char "c"
      ]
      undefined
  , DFunction
      "Builtin$.string$_bool_to_string"
      [ NK.Label NKC.bool "b"
      ]
      undefined
  , DFunction
      "Builtin$.nat$_unpack"
      [ NK.Label (NKT.TCon "nat" []) "nat"
      ]
      undefined
  , DFunction
      "Builtin$.nat$_pack"
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      "Builtin$.from_int32"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      "Builtin$.from_int64"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      "Builtin$.from_bignum"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      "Builtin$.negate"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      "Builtin$.(+)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      "Builtin$.(-)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      "Builtin$.(*)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(+)")
      [ NK.Label NKC.int32 "lhs"
      , NK.Label NKC.int32 "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(-)")
      [ NK.Label NKC.int32 "lhs"
      , NK.Label NKC.int32 "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(*)")
      [ NK.Label NKC.int32 "lhs"
      , NK.Label NKC.int32 "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "negate")
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(+)")
      [ NK.Label NKC.int64 "lhs"
      , NK.Label NKC.int64 "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(-)")
      [ NK.Label NKC.int64 "lhs"
      , NK.Label NKC.int64 "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(*)")
      [ NK.Label NKC.int64 "lhs"
      , NK.Label NKC.int64 "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "negate")
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(+)")
      [ NK.Label NKC.float "lhs"
      , NK.Label NKC.float "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(-)")
      [ NK.Label NKC.float "lhs"
      , NK.Label NKC.float "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(*)")
      [ NK.Label NKC.float "lhs"
      , NK.Label NKC.float "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "negate")
      [ NK.Label NKC.float "f"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(+)")
      [ NK.Label NKC.double "lhs"
      , NK.Label NKC.double "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(-)")
      [ NK.Label NKC.double "lhs"
      , NK.Label NKC.double "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(*)")
      [ NK.Label NKC.double "lhs"
      , NK.Label NKC.double "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "negate")
      [ NK.Label NKC.double "d"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int32")
      [ NK.Label NKC.int32 "m"
      ]
      undefined
  , -- TODO: fill in UNPARSED function
    DFunction
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int64")
      [ NK.Label NKC.int64 "m"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(+)")
      [ NK.Label (NKT.TCon "nat" []) "lhs"
      , NK.Label (NKT.TCon "nat" []) "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(-)")
      [ NK.Label (NKT.TCon "nat" []) "lhs"
      , NK.Label (NKT.TCon "nat" []) "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(*)")
      [ NK.Label (NKT.TCon "nat" []) "lhs"
      , NK.Label (NKT.TCon "nat" []) "rhs"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "negate")
      [ NK.Label (NKT.TCon "nat" []) "_"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(+)")
      [ NK.Label NKC.bignum "p"
      , NK.Label NKC.bignum "q"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(-)")
      [ NK.Label NKC.bignum "p"
      , NK.Label NKC.bignum "q"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(*)")
      [ NK.Label NKC.bignum "p"
      , NK.Label NKC.bignum "q"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "negate")
      [ NK.Label NKC.bignum "p"
      ]
      undefined
  , DFunction
      "Builtin$.compare"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      "Builtin$.(^)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "m"
      , NK.Label (NKT.TCon "nat" []) "n"
      ]
      undefined
  , DFunction
      "Builtin$.(<)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      undefined
  , DFunction
      "Builtin$.(<=)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      undefined
  , DFunction
      "Builtin$.(>)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      undefined
  , DFunction
      "Builtin$.(>=)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare")
      [ NK.Label NKC.int32 "x"
      , NK.Label NKC.int32 "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IInt64)) "compare")
      [ NK.Label NKC.int64 "x"
      , NK.Label NKC.int64 "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IFloat)) "compare")
      [ NK.Label NKC.float "x"
      , NK.Label NKC.float "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IDouble)) "compare")
      [ NK.Label NKC.double "x"
      , NK.Label NKC.double "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic INat)) "compare")
      [ NK.Label (NKT.TCon "nat" []) "x"
      , NK.Label (NKT.TCon "nat" []) "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IBool)) "compare")
      [ NK.Label NKC.bool "x"
      , NK.Label NKC.bool "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IChar)) "compare")
      [ NK.Label NKC.char "x"
      , NK.Label NKC.char "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IString)) "compare")
      [ NK.Label NKC.string "s1"
      , NK.Label NKC.string "s2"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TIntrinsic IBignum)) "compare")
      [ NK.Label NKC.bignum "x"
      , NK.Label NKC.bignum "y"
      ]
      undefined
  , DFunction
      "Builtin$.string$_length"
      [ NK.Label NKC.string "str"
      ]
      undefined
  , DFunction
      "Builtin$.string$_head_unsafe"
      [ NK.Label NKC.string "str"
      ]
      undefined
  , DFunction
      "Builtin$.string$_tail"
      [ NK.Label NKC.string "str"
      ]
      undefined
  , DFunction
      "Builtin$.string$_reverse"
      [ NK.Label NKC.string "str"
      ]
      undefined
  , DFunction
      "Builtin$.string$_remove_whitespace"
      [ NK.Label NKC.string "str"
      ]
      undefined
  , DFunction
      "Builtin$.string$_from_list"
      [ NK.Label (NKT.TCon "list" [NKC.char]) "chars"
      ]
      undefined
  , DFunction
      "Builtin$.string$_to_list"
      [ NK.Label NKC.string "str"
      ]
      undefined
  , DFunction
      "Builtin$.(==)"
      [ NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$a"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IInt32)) "(==)")
      [ NK.Label NKC.int32 "x"
      , NK.Label NKC.int32 "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IInt64)) "(==)")
      [ NK.Label NKC.int64 "x"
      , NK.Label NKC.int64 "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IFloat)) "(==)")
      [ NK.Label NKC.float "x"
      , NK.Label NKC.float "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IDouble)) "(==)")
      [ NK.Label NKC.double "x"
      , NK.Label NKC.double "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IBool)) "(==)")
      [ NK.Label NKC.bool "x"
      , NK.Label NKC.bool "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IChar)) "(==)")
      [ NK.Label NKC.char "x"
      , NK.Label NKC.char "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic INat)) "(==)")
      [ NK.Label (NKT.TCon "nat" []) "x"
      , NK.Label (NKT.TCon "nat" []) "y"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IString)) "(==)")
      [ NK.Label NKC.string "str1"
      , NK.Label NKC.string "str2"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.comparable (TIntrinsic IBignum)) "(==)")
      [ NK.Label NKC.bignum "m"
      , NK.Label NKC.bignum "n"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.divisible (TIntrinsic IFloat)) "(/)")
      [ NK.Label NKC.float "q"
      , NK.Label NKC.float "r"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.divisible (TIntrinsic IDouble)) "(/)")
      [ NK.Label NKC.double "q"
      , NK.Label NKC.double "r"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.modulo (TIntrinsic IInt32)) "(%)")
      [ NK.Label NKC.int32 "q"
      , NK.Label NKC.int32 "r"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.modulo (TIntrinsic IInt64)) "(%)")
      [ NK.Label NKC.int64 "q"
      , NK.Label NKC.int64 "r"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.modulo (TIntrinsic IBignum)) "(%)")
      [ NK.Label NKC.bignum "q"
      , NK.Label NKC.bignum "r"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.semigroup (TIntrinsic IString)) "(<>)")
      [ NK.Label NKC.string "s"
      , NK.Label NKC.string "t"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.semigroup (TApplication () (TConstructor () "List") (TVariable (Parameter () "a")))) "(<>)")
      [ NK.Label (NKT.TCon "list" [NKT.TOpq]) "xs"
      , NK.Label (NKT.TCon "list" [NKT.TOpq]) "ys"
      ]
      undefined
  , DFunction
      "Builtin$.io$_eval"
      [ NK.Label NKT.TOpq "v"
      ]
      undefined
  , DFunction
      "Builtin$.io$_return"
      [ NK.Label NKT.TOpq "v"
      ]
      undefined
  , DFunction
      "Builtin$.(!=)"
      [ NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$c"
      , NK.Label NKT.TOpq "a"
      , NK.Label NKT.TOpq "b"
      ]
      undefined
  , DFunction
      ( builtinInstance (Trait.comparable (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "(==)"
      )
      [ NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$a"
      , NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$b"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t1"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t2"
      ]
      undefined
  , DFunction
      (builtinInstance (Trait.ordered (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "compare")
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$b"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t1"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t2"
      ]
      undefined
  , DFunction
      "Builtin$.machine$_machine"
      [ NK.Label NKT.TOpq "seed"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq `NKC.arrow` NKT.TOpq) "transition"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "view"
      ]
      undefined
  , DFunction
      "Builtin$.machine$_map_machine"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label (NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq]) "m"
      ]
      undefined
  , DFunction
      "Builtin$.machine$_contramap_input"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label (NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq]) "m"
      ]
      undefined
  , DFunction
      "Builtin$.machine$_cofix"
      [ NK.Label (NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq] `NKC.arrow` NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq]) "f"
      ]
      undefined
  ]
