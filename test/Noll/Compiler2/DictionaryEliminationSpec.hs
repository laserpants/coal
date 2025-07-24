{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler2.DictionaryEliminationSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

fixtureFoo1 =
  undefined

--  EPlaceholderLambda
--    ()
--    ( Trait "Ordered" (TVariable (TypeIndex KType 0))
--        :| []
--    )
--    ( ELambda
--        ()
--        ( PVariable () (Label (TVariable (TypeIndex KType 0)) "m")
--            <| PVariable () (Label (TVariable (TypeIndex KType 0)) "n")
--            :| []
--        )
--        ( ECompiledMatch
--            ()
--            (TIntrinsic IBool)
--            ( EPlaceholderApplication
--                ()
--                (TConstructor KType "Ordering")
--                (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare")
--                ( Trait "Ordered" (TVariable (TypeIndex KType 0))
--                    :| []
--                )
--                [ EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
--                , EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
--                ]
--            )
--            ( ECompiledClause
--                (Label (TConstructor KType "Ordering") "EqualTo" :| [])
--                (ELiteral () (LBool True))
--                <| ECompiledClause
--                  (Label (TConstructor KType "Ordering") "GreaterThan" :| [])
--                  (ELiteral () (LBool False))
--                <| ECompiledClause
--                  (Label (TConstructor KType "Ordering") "LessThan" :| [])
--                  (ELiteral () (LBool True))
--                :| []
--            )
--        )
--    )

fixtureFoo2 =
  ELambda
    ()
    ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
        <| PVariable () (Label (TVariable (TypeIndex KType 0)) "m")
        <| PVariable () (Label (TVariable (TypeIndex KType 0)) "n")
        :| []
    )
    ( ECompiledMatch
        ()
        (TIntrinsic IBool)
        ( EApplication
            ()
            (TConstructor KType "Ordering")
            (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
            ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                :| []
            )
        )
        ( ECompiledClause
            (Label (TConstructor KType "Ordering") "EqualTo" :| [])
            (ELiteral () (LBool True))
            <| ECompiledClause
              (Label (TConstructor KType "Ordering") "GreaterThan" :| [])
              (ELiteral () (LBool False))
            <| ECompiledClause
              (Label (TConstructor KType "Ordering") "LessThan" :| [])
              (ELiteral () (LBool True))
            :| []
        )
    )
