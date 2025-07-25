{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystemFixtures.Expression where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Language

--
-- let
--   lte =
--     fn(x) =>
--       fn(y) =>
--         match(compare(x, y)) {
--           | LessThan or EqualTo => true
--           | GreaterThan => false
--         }
--   in
--     lte
--
expression1 :: Expression () ()
expression1 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "lte"))
        ( ELambda
            ()
            (PVariable () (Label () "x") :| [])
            ( ELambda
                ()
                (PVariable () (Label () "y") :| [])
                ( EMatch
                    ()
                    ()
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "compare"))
                        (EVariable () (Label () "x") <| EVariable () (Label () "y") :| [])
                    )
                    ( EClause
                        ()
                        ( POr
                            ()
                            ()
                            (PConstructor () (Label () "LessThan") [])
                            (PConstructor () (Label () "EqualTo") [])
                        )
                        (CPlain () [] (ELiteral () (LBool True)) :| [])
                        <| EClause
                          ()
                          (PConstructor () (Label () "GreaterThan") [])
                          (CPlain () [] (ELiteral () (LBool False)) :| [])
                          :| []
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label () "lte"))

expression2 :: Expression () (Type TypeIndex Kind)
expression2 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "lte"))
        ( ELambda
            ()
            (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
            ( ELambda
                ()
                (PVariable () (Label (TVariable (TypeIndex KType 0)) "y") :| [])
                ( EMatch
                    ()
                    (TIntrinsic IBool)
                    ( EApplication
                        ()
                        (TConstructor KType "Ordering")
                        (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                        (EVariable () (Label (TVariable (TypeIndex KType 0)) "x") <| EVariable () (Label (TVariable (TypeIndex KType 0)) "y") :| [])
                    )
                    ( EClause
                        ()
                        ( POr
                            ()
                            (TConstructor KType "Ordering")
                            (PConstructor () (Label (TConstructor KType "Ordering") "LessThan") [])
                            (PConstructor () (Label (TConstructor KType "Ordering") "EqualTo") [])
                        )
                        (CPlain () [] (ELiteral () (LBool True)) :| [])
                        <| EClause
                          ()
                          (PConstructor () (Label (TConstructor KType "Ordering") "GreaterThan") [])
                          (CPlain () [] (ELiteral () (LBool False)) :| [])
                          :| []
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "lte"))
