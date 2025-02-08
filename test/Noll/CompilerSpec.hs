{-# LANGUAGE OverloadedStrings #-}

module Noll.CompilerSpec where

import Control.Monad.Identity (runIdentity)
import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Definition (..),
  Expression (..),
  Function (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Module (..),
  Parameter (..),
  Path (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Trait (..),
  Type (..),
  TypeIndex (..),
  Uses (..),
 )
import Noll.SystemF (
  Assumption (..),
  Constraint (..),
  ConstraintsGenContext (..),
  ConstraintsGenError (..),
  ConstraintsGenOutput,
  ConstraintsGenStack (..),
  ConstraintsGenState (..),
  InferenceRule (..),
  Substitutable (..),
  Substitution (..),
  checkTypeAnnotationParameters,
  collectConstraints,
  normalizeTypeIndexes,
  runConstraintsGenStack,
  solveConstraints,
 )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      1 == 2

foo =
  runIdentity $
    runCompilerT
      ( CompilerEnvironment
          ( Environment.fromList
              [
                ( "LessThan"
                , Constructor
                    "LessThan"
                    0
                    (Forall mempty [] (TConstructor KType "Ordering"))
                )
              ,
                ( "GreaterThan"
                , Constructor
                    "GreaterThan"
                    0
                    (Forall mempty [] (TConstructor KType "Ordering"))
                )
              ,
                ( "EqualTo"
                , Constructor
                    "EqualTo"
                    0
                    (Forall mempty [] (TConstructor KType "Ordering"))
                )
              ]
          )
          ( Environment.fromList
              []
          )
      )
      baz

baz :: (Monad m) => CompilerT () m (Function Expression () IndexedType, [CompilerAssumption])
baz = do
  insertNamesC
    [
      ( "compare"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TConstructor KType "Ordering"
          )
      )
    ,
      ( "not"
      , Forall
          mempty
          []
          (TIntrinsic IBool `TArrow` TIntrinsic IBool)
      )
    ,
      ( "less_than_or_equal_to"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ]
  e <-
    indexedC $
      Function
        ()
        (Uses [] ())
        ( PAnnotation
            ()
            (TVariable (Parameter () "a"))
            (PVariable () (Label () "n"))
            :| []
        )
        ( EApplication
            ()
            ()
            (EBinaryOperator () ((), OReverseComposition))
            ( EVariable () (Label () "not")
                <| EApplication
                  ()
                  ()
                  (EVariable () (Label () "less_than_or_equal_to"))
                  (EVariable () (Label () "n") :| [])
                :| []
            )
        )
  (xx, yy) <- typeCheckFunctionC e
  pure (normalizeTypeIndexes xx, yy)
