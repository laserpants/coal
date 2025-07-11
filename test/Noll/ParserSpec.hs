{-# LANGUAGE OverloadedStrings #-}

module Noll.ParserSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Expression
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (runParser)

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      runParser expressionParser "" "fold(pack_nat(n)) { | Zero => 1 | Succ(@f) as m => unpack_nat(m) * f }"
        == Right
          ( EFold
              ()
              ()
              ( EApplication
                  ()
                  ()
                  (EVariable () (Label () "pack_nat"))
                  ( EVariable () (Label () "n")
                      :| []
                  )
                  :| []
              )
              ( EClause
                  ()
                  ( PConstructor
                      ()
                      (Label () "Zero")
                      []
                  )
                  ( CPlain
                      ()
                      []
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "from_int32"))
                          (ELiteral () (LInt32 1) :| [])
                      )
                      :| []
                  )
                  <| EClause
                    ()
                    ( PAs
                        ()
                        (Label () "m")
                        ( PConstructor
                            ()
                            (Label () "Succ")
                            [ PAtVariable () (Label () "f")
                            ]
                        )
                    )
                    ( CPlain
                        ()
                        []
                        ( EApplication
                            ()
                            ()
                            (EBinaryOperator () () OMultiplication)
                            ( EApplication
                                ()
                                ()
                                (EVariable () (Label () "unpack_nat"))
                                ( EVariable () (Label () "m")
                                    :| []
                                )
                                <| EVariable () (Label () "f")
                                :| []
                            )
                        )
                        :| []
                    )
                  :| []
              )
              Nothing
          )
