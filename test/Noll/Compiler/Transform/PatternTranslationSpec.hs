{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.PatternTranslationSpec where

import Noll.Common.List1 (NonEmpty ((:|)))
import Noll.Compiler.Transform.PatternTranslation (runTranslate, translate)
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Choice (..), Clause (..), Expression (..), Intrinsic (..), Pattern (..), Type (..), TypeIndex (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "" $ do
    it "" $
      runTranslate "v" 0 (translate fixture1) == fixture2
    it "" $
      runTranslate "v" 0 (translate fixture3) == fixture4
    it "" $
      runTranslate "v" 0 (translate fixture5) == fixture6

bazSpec :: Expression () (Type TypeIndex ())
bazSpec =
  runTranslate "foo" 0 $
    translate
      fixture1

-- let
--  Some(p) =       Option(int32)
--    x             Option(int32)
--  in
--    p             int32
--
fixture1 :: Expression () (Type TypeIndex ())
fixture1 =
  ( ELet
      ()
      ( BPattern
          ()
          (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          :| []
      )
      (EVariable () (Label (TIntrinsic IInt32) "p"))
  )

-- let
--  $v.0 =               Option(int32)
--    x                  Option(int32)
--  in
--    match(             int32
--      $v.0             Option(int32)
--    ) {
--      | Some(p) =>     Option(int32)
--          p            int32
--    }
--
fixture2 :: Expression () (Type TypeIndex ())
fixture2 =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          :| []
      )
      ( EMatch
          ()
          (TIntrinsic IInt32)
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          ( EClause
              ()
              (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
              ( CPlain
                  ()
                  []
                  (EVariable () (Label (TIntrinsic IInt32) "p"))
                  :| []
              )
              :| []
          )
      )
  )

-- fn(Some(p)) =>        Option(int32)
--   p                   int32
--
fixture3 :: Expression () (Type TypeIndex ())
fixture3 =
  ELambda
    ()
    (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")] :| [])
    (EVariable () (Label (TIntrinsic IInt32) "p"))

-- fn($v.0) =>           Option(int32)
--   match(              int32
--     $v.0              Option(int32)
--   ) {
--     | Some(p) =>      Option(int32)
--         p             int32
--   }
--
fixture4 :: Expression () (Type TypeIndex ())
fixture4 =
  ELambda
    ()
    (PVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0") :| [])
    ( EMatch
        ()
        (TIntrinsic IInt32)
        (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
        ( EClause
            ()
            (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
            ( CPlain
                ()
                []
                (EVariable () (Label (TIntrinsic IInt32) "p"))
                :| []
            )
            :| []
        )
    )

-- letrec
--  Some(p) =       Option(int32)
--    x             Option(int32)
--  in
--    p             int32
--
fixture5 :: Expression () (Type TypeIndex ())
fixture5 =
  ( ERecursiveLet
      ()
      (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
      (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
      (EVariable () (Label (TIntrinsic IInt32) "p"))
  )

-- let
--  $v.0 =               Option(int32)
--    x                  Option(int32)
--  in
--    match(             int32
--      $v.0             Option(int32)
--    ) {
--      | Some(p) =>     Option(int32)
--          p            int32
--    }
--
fixture6 :: Expression () (Type TypeIndex ())
fixture6 =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "x"))
          :| []
      )
      ( EMatch
          ()
          (TIntrinsic IInt32)
          (EVariable () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "$v.0"))
          ( EClause
              ()
              (PConstructor () (Label (TApplication () (TConstructor () "Option") (TIntrinsic IInt32 :| [])) "Some") [PVariable () (Label (TIntrinsic IInt32) "p")])
              ( CPlain
                  ()
                  []
                  (EVariable () (Label (TIntrinsic IInt32) "p"))
                  :| []
              )
              :| []
          )
      )
  )
