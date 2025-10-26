{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.GenerationSpec (collectExampleConstraintsSpec1) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.State
import Coal.Language
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.Internal
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map

collectExampleConstraintsSpec1 :: ([CompilerAssumption ()], [ConstraintsGenOutput () TypeIndex Kind IndexedType])
collectExampleConstraintsSpec1 = (ms, outs)
 where
  (ms, outs) = evalConstraintsGenStack (freshIdIn expr) ctx (emitConstraints expr)
  expr = fixture1
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      , constraintsGenContextTopLevelFoldEnv = mempty
      }

--    let
--      print_name =
--        fn(b : { name : string | q }) =>
--          trace_string(b.name)
--        in
--          match(a : { name : string | r }) {
--            | { name = name | t } =>
--                print_name(t)
--          };

fixture1 :: Expression () IndexedType
fixture1 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (TVariable (TypeIndex KType 0)) "print_name"))
        ( ELambda
            ()
            ( PAnnotation
                ()
                ( TIntrinsic
                    ( IRecord
                        ( TRow (RExtend "name" (TIntrinsic IString) (RVariable (Parameter () "q")))
                        )
                    )
                )
                (PVariable () (Label (TVariable (TypeIndex KType 1)) "b"))
                :| []
            )
            ( EApplication
                ()
                (TVariable (TypeIndex KType 2))
                (EVariable () (Label (TVariable (TypeIndex KType 3)) "trace_string"))
                ( ESelect
                    ()
                    (Label (TVariable (TypeIndex KType 4)) "name")
                    ( EVariable () (Label (TVariable (TypeIndex KType 5)) "b")
                    )
                    :| []
                )
            )
        )
        :| []
    )
    ( EMatch
        ()
        (TVariable (TypeIndex KType 6))
        ( EAnnotation
            ()
            ( TIntrinsic
                ( IRecord
                    ( TRow (RExtend "name" (TIntrinsic IString) (RVariable (Parameter () "r")))
                    )
                )
            )
            (EVariable () (Label (TVariable (TypeIndex KType 7)) "a"))
        )
        ( EClause
            ()
            ( PRecord
                ()
                (TVariable (TypeIndex KType 8))
                ( Map.fromList
                    [
                      ( "name"
                      , PVariable () (Label (TVariable (TypeIndex KType 9)) "name")
                      )
                    ]
                )
                (Just (PVariable () (Label (TVariable (TypeIndex KType 10)) "t")))
            )
            ( CPlain
                ()
                []
                ( EApplication
                    ()
                    (TVariable (TypeIndex KType 11))
                    (EVariable () (Label (TVariable (TypeIndex KType 12)) "print_name"))
                    (EVariable () (Label (TVariable (TypeIndex KType 13)) "t") :| [])
                )
                :| []
            )
            :| []
        )
    )
