{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemFixtures.Expression where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Label (Label (..))
import Noll.Language

-- 
-- data List
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
