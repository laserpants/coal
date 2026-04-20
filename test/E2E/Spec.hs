{-# LANGUAGE OverloadedStrings #-}

module E2E.Spec (e2eSpec, runSpec) where

import Coal.Compiler (pipeline)
import Coal.Compiler.Config (CompilerConfig (..), defaultConfig)
import Coal.Compiler.Environment
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import System.Process
import Test.Hspec

e2eSpec :: Spec
e2eSpec = do
  describe "001" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/001" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "002" $
    expectOutput
      "24"
      "test/Coal/examples/002"
      [ "Main.coal"
      ]

  describe "003" $
    expectOutput
      "1"
      "test/Coal/examples/003"
      [ "Main.coal"
      ]

  describe "004" $
    expectOutput
      "2"
      "test/Coal/examples/004"
      [ "Main.coal"
      ]

  describe "005" $
    expectOutput
      "40320"
      "test/Coal/examples/005"
      [ "Math.coal"
      , "Main.coal"
      ]

  describe "006" $
    expectOutput
      "101"
      "test/Coal/examples/006"
      [ "Tree.coal"
      , "Qsort.coal"
      , "Main.coal"
      ]

  describe "007" $
    expectOutput
      "hello"
      "test/Coal/examples/007"
      [ "Main.coal"
      ]

  describe "008" $
    expectOutput
      "cluedo"
      "test/Coal/examples/008"
      [ "Main.coal"
      ]

  describe "009" $
    expectOutput
      "wat"
      "test/Coal/examples/009"
      [ "Main.coal"
      ]

  describe "010" $
    expectOutput
      "hello from the other side"
      "test/Coal/examples/010"
      ["Main.coal"]

  describe "011" $
    expectOutput
      "Covfefe"
      "test/Coal/examples/011"
      ["Main.coal"]

  describe "012" $
    expectOutput
      "bork bork bork"
      "test/Coal/examples/012"
      ["Main.coal"]

  describe "013" $ do
    it "is TypeError" $ do
      res <-
        runSpec
          "test/Coal/examples/013"
          [ "Main.coal"
          ]
      res `shouldBe` Left TypeError

  describe "014" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/014" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "015" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/015" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "016" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/016" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "017" $
    expectOutput "false" "test/Coal/examples/017" ["Main.coal"]

  describe "018" $
    expectOutput
      "40320"
      "test/Coal/examples/018"
      [ "Main.coal"
      ]

  describe "019" $
    expectOutput "Bob" "test/Coal/examples/019" ["Main.coal"]

  describe "020" $
    expectOutput "Lazarus" "test/Coal/examples/020" ["Main.coal"]

  describe "021" $
    expectOutput "Alphonso" "test/Coal/examples/021" ["Main.coal"]

  describe "022" $
    expectOutput
      "1234"
      "test/Coal/examples/022"
      [ "Main.coal"
      ]

  describe "023" $
    expectOutput
      "123"
      "test/Coal/examples/023"
      [ "Main.coal"
      ]

  describe "024" $
    expectOutput
      "x"
      "test/Coal/examples/024"
      [ "Main.coal"
      ]

  describe "025" $ do
    it "is NoSuchIdentifier" $ do
      res <- runSpec "test/Coal/examples/025" ["Main.coal"]
      res `shouldBe` Left NoSuchIdentifier

  describe "026" $
    expectOutput "5" "test/Coal/examples/026" ["Main.coal"]

  describe "027" $
    expectOutput
      "ananab"
      "test/Coal/examples/027"
      [ "Main.coal"
      ]

  describe "028" $
    expectOutput "2" "test/Coal/examples/028" ["Main.coal"]

  describe "029" $
    expectOutput "5" "test/Coal/examples/029" ["Main.coal"]

  describe "030" $
    expectOutput
      "123"
      "test/Coal/examples/030"
      [ "Main.coal"
      ]

  describe "031" $
    expectOutput
      "111\n111"
      "test/Coal/examples/031"
      [ "Main.coal"
      ]

  describe "032" $
    expectOutput
      "9876"
      "test/Coal/examples/032"
      [ "Main.coal"
      ]

  describe "033" $
    expectOutput
      "-123"
      "test/Coal/examples/033"
      [ "Main.coal"
      ]

  describe "034" $
    expectOutput
      "59876"
      "test/Coal/examples/034"
      [ "Main.coal"
      ]

  describe "035" $
    expectOutput "true" "test/Coal/examples/035" ["Main.coal"]

  describe "036" $
    expectOutput "true" "test/Coal/examples/036" ["Main.coal"]

  describe "037" $
    expectOutput "true" "test/Coal/examples/037" ["Main.coal"]

  describe "038" $
    expectOutput "true" "test/Coal/examples/038" ["Main.coal"]

  describe "039" $
    expectOutput "2" "test/Coal/examples/039" ["Main.coal"]

  describe "040" $
    expectOutput "6" "test/Coal/examples/040" ["Main.coal"]

  describe "041" $
    expectOutput "512" "test/Coal/examples/041" ["Main.coal"]

  describe "042" $
    expectOutput "8" "test/Coal/examples/042" ["Main.coal"]

  describe "043" $
    expectOutput "1" "test/Coal/examples/043" ["Main.coal"]

  describe "044" $
    expectOutput "false" "test/Coal/examples/044" ["Main.coal"]

  describe "045" $
    expectOutput "1" "test/Coal/examples/045" ["Main.coal"]

  describe "046" $
    expectOutput "22.500000" "test/Coal/examples/046" ["Main.coal"]

  describe "047" $
    expectOutput "23.000000000000000" "test/Coal/examples/047" ["Main.coal"]

  describe "048" $
    expectOutput "123" "test/Coal/examples/048" ["Main.coal"]

  describe "049" $ do
    it "is CallCycleError" $ do
      res <- runSpec "test/Coal/examples/049" ["Main.coal"]
      res `shouldBe` Left CallCycleError

  describe "050" $
    expectOutput
      "720"
      "test/Coal/examples/050"
      [ "Main.coal"
      ]

  describe "051" $
    expectOutput "Prot" "test/Coal/examples/051" ["Main.coal"]

  describe "052" $
    expectOutput "prot" "test/Coal/examples/052" ["Main.coal"]

  describe "053" $
    expectOutput "Wat" "test/Coal/examples/053" ["Main.coal"]

  describe "054" $
    expectOutput "hello world" "test/Coal/examples/054" ["Main.coal"]

  describe "055" $
    expectOutput "Covfefe" "test/Coal/examples/055" ["Main.coal"]

  describe "056" $
    expectOutput "true" "test/Coal/examples/056" ["Main.coal"]

  describe "057" $
    expectOutput "Lorenzo" "test/Coal/examples/057" ["Main.coal"]

  describe "058" $
    expectOutput "Lorenzo" "test/Coal/examples/058" ["Main.coal"]

  describe "059" $
    expectOutput "a" "test/Coal/examples/059" ["Main.coal"]

  describe "060" $
    expectOutput "true" "test/Coal/examples/060" ["Main.coal"]

  describe "061" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/061" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "062" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/062" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "063" $
    expectOutput
      "24"
      "test/Coal/examples/063"
      [ "Main.coal"
      ]

  describe "064" $
    expectOutput "6" "test/Coal/examples/064" ["Main.coal"]

  describe "065" $
    expectOutput "true" "test/Coal/examples/065" ["Main.coal"]

  describe "066" $
    expectOutput
      "7"
      "test/Coal/examples/066"
      [ "Main.coal"
      , "MyList.coal"
      ]

  describe "067" $
    expectOutput
      "{\"abc\":[\"a\",\"b\",\"c\"],\"pi\":3.14159}"
      "test/Coal/examples/067"
      [ "StringUtils.coal"
      , "MyList.coal"
      , "Main.coal"
      ]

  describe "068" $
    expectOutput "3" "test/Coal/examples/068" ["Main.coal"]

  describe "069" $
    expectOutput "512" "test/Coal/examples/069" ["Main.coal"]

  describe "070" $
    expectOutput "2" "test/Coal/examples/070" ["Main.coal"]

  describe "071" $
    expectOutput
      "100"
      "test/Coal/examples/071"
      [ "Main.coal"
      ]

  describe "072" $
    expectOutput "2" "test/Coal/examples/072" ["Main.coal"]

  describe "075" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec "test/Coal/examples/075" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "076" $
    expectOutput "1" "test/Coal/examples/076" ["Main.coal"]

  describe "077" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec "test/Coal/examples/077" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "078" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec "test/Coal/examples/078" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "079" $
    expectOutput "5" "test/Coal/examples/079" ["Main.coal"]

  describe "080" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/080" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "081" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec "test/Coal/examples/081" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "082" $ do
    expectOutput "3" "test/Coal/examples/082" ["Main.coal"]

  describe "083" $ do
    expectOutput "4" "test/Coal/examples/083" ["Main.coal"]

  describe "084" $ do
    expectOutput "false" "test/Coal/examples/084" ["Main.coal"]

  describe "085" $ do
    expectOutput "-5" "test/Coal/examples/085" ["Main.coal"]

  describe "086" $ do
    expectOutput "1" "test/Coal/examples/086" ["Main.coal"]

  describe "087" $ do
    expectOutput "4" "test/Coal/examples/087" ["Main.coal"]

  describe "088" $ do
    expectOutput "5" "test/Coal/examples/088" ["Main.coal"]

  describe "089" $ do
    expectOutput "3" "test/Coal/examples/089" ["Main.coal"]

  describe "090" $ do
    expectOutput "3.000000000000000" "test/Coal/examples/090" ["Main.coal"]

  describe "091" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec "test/Coal/examples/091" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "092" $ do
    expectOutput "123" "test/Coal/examples/092" ["Main.coal"]

  describe "093" $ do
    expectOutput "321" "test/Coal/examples/093" ["Main.coal"]

  describe "094" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec "test/Coal/examples/094" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "096" $ do
    expectOutput "Hello Space" "test/Coal/examples/096" ["Main.coal"]

  describe "098" $ do
    expectOutput "-627128164" "test/Coal/examples/098" ["Main.coal"]

  describe "099" $ do
    expectOutput
      "8"
      "test/Coal/examples/099"
      [ "Main.coal"
      ]

  describe "100" $ do
    expectOutput "4.323232444322323" "test/Coal/examples/100" ["Main.coal"]

  describe "101" $ do
    expectOutput "4.130000" "test/Coal/examples/101" ["Main.coal"]

  describe "102" $ do
    expectOutput "3" "test/Coal/examples/102" ["Main.coal"]

  describe "103" $ do
    expectOutput "1" "test/Coal/examples/103" ["Main.coal"]

  describe "104" $ do
    expectOutput "6" "test/Coal/examples/104" ["Main.coal"]

  describe "105" $ do
    it "is TraitError" $ do
      res <- runSpec "test/Coal/examples/105" ["Main.coal"]
      res `shouldBe` Left TraitError

  --  describe "106" $ do
  --    it "is PreflightFailure" $ do
  --      res <- runSpec "test/Coal/examples/106" ["Main.coal"]
  --      res `shouldBe` Left PreflightFailure

  describe "107" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/107" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "108" $ do
    it "is MissingMainEntryPoint" $ do
      res <- runSpec "test/Coal/examples/108" ["Main.coal"]
      res `shouldBe` Left MissingMainEntryPoint

  describe "109" $ do
    expectOutput "Hello, world!" "test/Coal/examples/109" ["Main.coal"]

  describe "110" $ do
    expectOutput "d" "test/Coal/examples/110" ["Main.coal"]

  describe "111" $ do
    expectOutput "b" "test/Coal/examples/111" ["Main.coal"]

  describe "112" $ do
    expectOutput "5" "test/Coal/examples/112" ["Main.coal"]

  describe "113" $ do
    expectOutput "a" "test/Coal/examples/113" ["Main.coal"]

  describe "114" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/114" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "115" $ do
    expectOutput "b" "test/Coal/examples/115" ["Main.coal"]

  describe "116" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/116" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "117" $ do
    expectOutput "4" "test/Coal/examples/117" ["Main.coal"]

  describe "118" $ do
    expectOutput "hellohello" "test/Coal/examples/118" ["Main.coal"]

  describe "119" $ do
    expectOutput "hello" "test/Coal/examples/119" ["Main.coal"]

  describe "120" $ do
    expectOutput "true" "test/Coal/examples/120" ["Main.coal"]

  describe "121" $ do
    expectOutput
      "342"
      "test/Coal/examples/121"
      [ "Main.coal"
      ]

  describe "122" $ do
    it "is NoSuchIdentifier" $ do
      res <-
        runSpec
          "test/Coal/examples/122"
          [ "Main.coal"
          ]
      res `shouldBe` Left NoSuchIdentifier

  describe "123" $ do
    expectOutput
      "4.1"
      "test/Coal/examples/123"
      [ "Main.coal"
      ]

  describe "124" $ do
    expectOutput
      "ail"
      "test/Coal/examples/124"
      [ "Main.coal"
      ]

  describe "125" $ do
    expectOutput
      "11"
      "test/Coal/examples/125"
      [ "Main.coal"
      ]

  describe "126" $ do
    expectOutput
      "h"
      "test/Coal/examples/126"
      [ "Main.coal"
      ]

  --  describe "127" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/127"
  --          [ "Main.coal"
  --          , "Foo.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure
  --
  --  describe "128" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/128"
  --          [ "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "129" $ do
    expectOutput
      "Hello, World!\n"
      "test/Coal/examples/129"
      [ "Main.coal"
      ]

  describe "131" $ do
    expectOutput
      "🚀"
      "test/Coal/examples/131"
      [ "Main.coal"
      ]

  describe "136" $
    expectOutput
      "101"
      "test/Coal/examples/136"
      [ "Tree.coal"
      , "Qsort.coal"
      , "Main.coal"
      ]

  describe "137" $
    expectOutput
      "2"
      "test/Coal/examples/137"
      [ "Eq.coal"
      , "Stuff.coal"
      , "Main.coal"
      ]

  describe "138" $
    expectOutput
      "3"
      "test/Coal/examples/138"
      [ "Eq.coal"
      , "Stuff.coal"
      , "Main.coal"
      ]

  describe "139" $ do
    it "is TraitError" $ do
      res <-
        runSpec
          "test/Coal/examples/139"
          [ "Eq.coal"
          , "Stuff.coal"
          , "Main.coal"
          ]
      res `shouldBe` Left TraitError

  describe "140" $ do
    it "is TypeError" $ do
      res <-
        runSpec
          "test/Coal/examples/140"
          [ "Eq.coal"
          , "Stuff.coal"
          , "Main.coal"
          ]
      res `shouldBe` Left TypeError

  --  describe "141" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/141"
  --          [ "Eq.coal"
  --          , "Stuff.coal"
  --          , "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure
  --
  --  describe "142" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/142"
  --          [ "Eq.coal"
  --          , "Stuff.coal"
  --          , "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure
  --
  --  describe "143" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/143"
  --          [ "Eq.coal"
  --          , "Stuff.coal"
  --          , "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  --  describe "144" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/144"
  --          [ "Eq.coal"
  --          , "Stuff.coal"
  --          , "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure
  --
  --  describe "145" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/145"
  --          [ "Eq.coal"
  --          , "Stuff.coal"
  --          , "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "146" $ do
    expectOutput
      "223"
      "test/Coal/examples/146"
      [ "Eq.coal"
      , "Stuff.coal"
      , "Main.coal"
      ]

  describe "147" $ do
    expectOutput
      "224"
      "test/Coal/examples/147"
      [ "Eq.coal"
      , "Stuff.coal"
      , "Main.coal"
      ]

  describe "148" $ do
    expectOutput
      "224"
      "test/Coal/examples/148"
      [ "Main.coal"
      ]

  describe "149" $ do
    it "is NoSuchIdentifier" $ do
      res <-
        runSpec
          "test/Coal/examples/149"
          [ "Eq.coal"
          , "Stuff.coal"
          , "Main.coal"
          ]
      res `shouldBe` Left NoSuchIdentifier

  describe "150" $ do
    expectOutput
      "255"
      "test/Coal/examples/150"
      [ "Eq.coal"
      , "Stuff.coal"
      , "Main.coal"
      ]

  describe "151" $ do
    expectOutput
      "22"
      "test/Coal/examples/151"
      [ "Main.coal"
      ]

  describe "152" $ do
    expectOutput
      "5.500000"
      "test/Coal/examples/152"
      [ "Main.coal"
      ]

  describe "155" $ do
    expectOutput
      "Wat"
      "test/Coal/examples/155"
      [ "Main.coal"
      ]

  describe "156" $ do
    expectOutput
      "1"
      "test/Coal/examples/156"
      [ "Main.coal"
      ]

  describe "158" $ do
    expectOutput
      "aabc"
      "test/Coal/examples/158"
      [ "Main.coal"
      , "Json.coal"
      ]

  --  describe "159" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/159"
  --          [ "Hello.coal"
  --          , "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "160" $ do
    expectOutput
      "hello"
      "test/Coal/examples/160"
      [ "Main.coal"
      ]

  describe "161" $ do
    expectOutput
      "true\nfalse"
      "test/Coal/examples/161"
      [ "Main.coal"
      ]

  --  describe "163" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/163"
  --          [ "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "166" $ do
    expectOutput
      "42"
      "test/Coal/examples/166"
      [ "Main.coal"
      ]

  describe "167" $ do
    expectOutput
      "42"
      "test/Coal/examples/167"
      [ "Main.coal"
      ]

  describe "169" $ do
    expectOutput
      "-3.000000"
      "test/Coal/examples/169"
      [ "Main.coal"
      ]

  describe "172" $ do
    expectOutput
      "5.000000000000000"
      "test/Coal/examples/172"
      [ "Main.coal"
      ]

  describe "173" $ do
    expectOutput
      "5.000000"
      "test/Coal/examples/173"
      [ "Main.coal"
      ]

  describe "174" $ do
    it "is TypeError" $ do
      res <-
        runSpec
          "test/Coal/examples/174"
          [ "Main.coal"
          ]
      res `shouldBe` Left TypeError

  describe "175" $ do
    expectOutput
      "10"
      "test/Coal/examples/175"
      [ "Main.coal"
      ]

  describe "176" $ do
    expectOutput
      "one\ntwo\nthree"
      "test/Coal/examples/176"
      [ "Main.coal"
      ]

  describe "178" $ do
    expectOutput
      "Authentication failed"
      "test/Coal/examples/178"
      [ "Main.coal"
      ]

  describe "179" $ do
    expectOutput
      "aaa"
      "test/Coal/examples/179"
      [ "Main.coal"
      ]

  describe "180" $ do
    expectOutput
      "abc"
      "test/Coal/examples/180"
      [ "Main.coal"
      ]

  describe "181" $ do
    expectOutput
      "4"
      "test/Coal/examples/181"
      [ "Main.coal"
      ]

  describe "182" $ do
    expectOutput
      "224"
      "test/Coal/examples/182"
      [ "Main.coal"
      ]

  describe "183" $ do
    expectOutput
      "-5"
      "test/Coal/examples/183"
      [ "Main.coal"
      ]

  --  describe "184" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/184"
  --          [ "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure
  --
  --  describe "185" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/185"
  --          [ "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "186" $ do
    expectOutput
      "true"
      "test/Coal/examples/186"
      [ "Main.coal"
      ]

  describe "187" $ do
    it "is ParserFailure" $ do
      res <-
        runSpec
          "test/Coal/examples/187"
          [ "Main.coal"
          ]
      res `shouldBe` Left ParserFailure

  describe "188" $ do
    expectOutput
      "abc"
      "test/Coal/examples/188"
      [ "Main.coal"
      ]

  describe "189" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/189" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "190" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/190" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "194" $ do
    expectOutput
      "🚀Hello"
      "test/Coal/examples/194"
      [ "Main.coal"
      , "Stuff.coal"
      ]

  --  describe "195" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/195"
  --          [ "Main.coal"
  --          , "Stuff.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "196" $ do
    expectOutput
      "8\n55\n101\n102\n103\n104\n105\n106\n107\n109\n234\n999"
      "test/Coal/examples/196"
      [ "Main.coal"
      , "Tree.coal"
      , "Qsort.coal"
      ]

  --  describe "197" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/197"
  --          [ "Main.coal"
  --          , "Foo.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "198" $ do
    it "is PreflightFailure" $ do
      res <-
        runSpec
          "test/Coal/examples/198"
          [ "Main.coal"
          , "Foo.coal"
          , "Baz.coal"
          ]
      res `shouldBe` Left PreflightFailure

  --  describe "200" $ do
  --    it "is PreflightFailure" $ do
  --      res <-
  --        runSpec
  --          "test/Coal/examples/200"
  --          [ "Main.coal"
  --          ]
  --      res `shouldBe` Left PreflightFailure

  describe "201" $ do
    expectOutput
      "123"
      "test/Coal/examples/201"
      [ "Main.coal"
      ]

  describe "202" $ do
    expectOutput
      "4"
      "test/Coal/examples/202"
      [ "Main.coal"
      ]

  describe "205" $ do
    expectOutput
      "Merry Christmas!"
      "test/Coal/examples/205"
      [ "Main.coal"
      ]

  describe "206" $ do
    expectOutput
      "5"
      "test/Coal/examples/206"
      [ "Main.coal"
      ]

  describe "207" $ do
    expectOutput
      "Banan"
      "test/Coal/examples/207"
      [ "Main.coal"
      ]

  describe "208" $ do
    it "is PreflightFailure" $ do
      res <-
        runSpec
          "test/Coal/examples/208"
          [ "Main.coal"
          ]
      res `shouldBe` Left PreflightFailure

  describe "210" $ do
    expectOutput
      "!"
      "test/Coal/examples/210"
      [ "Main.coal"
      ]

  describe "211" $ do
    expectOutput
      "F"
      "test/Coal/examples/211"
      [ "Main.coal"
      ]

  describe "212" $ do
    expectOutput
      "F"
      "test/Coal/examples/212"
      [ "Main.coal"
      ]

  describe "213" $ do
    expectOutput
      "g"
      "test/Coal/examples/213"
      [ "Main.coal"
      ]

  describe "214" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/214" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "215" $ do
    expectOutput
      "6"
      "test/Coal/examples/215"
      [ "Main.coal"
      ]

  describe "216" $ do
    expectOutput
      "5"
      "test/Coal/examples/216"
      [ "Main.coal"
      ]

  describe "217" $ do
    expectOutput
      "File not found"
      "test/Coal/examples/217"
      [ "Main.coal"
      ]

  describe "218" $ do
    expectOutput
      "cool"
      "test/Coal/examples/218"
      [ "Main.coal"
      ]

  describe "219" $ do
    expectOutput
      "yes"
      "test/Coal/examples/219"
      [ "Main.coal"
      ]

  describe "220" $ do
    expectOutput
      "true"
      "test/Coal/examples/220"
      [ "Main.coal"
      ]

  describe "221" $ do
    expectOutput
      "123"
      "test/Coal/examples/221"
      [ "Main.coal"
      ]

  describe "222" $ do
    expectOutput
      "no"
      "test/Coal/examples/222"
      [ "Main.coal"
      ]

  describe "223" $ do
    expectOutput
      "11111119223372036854775808"
      "test/Coal/examples/223"
      [ "Main.coal"
      ]

  describe "224" $ do
    expectOutput
      "6"
      "test/Coal/examples/224"
      [ "Main.coal"
      ]

  describe "226" $ do
    expectOutput
      "true"
      "test/Coal/examples/226"
      [ "Main.coal"
      ]

  describe "227" $ do
    expectOutput
      "false"
      "test/Coal/examples/227"
      [ "Main.coal"
      ]

  describe "228" $ do
    expectOutput
      "LessThan"
      "test/Coal/examples/228"
      [ "Main.coal"
      ]

  describe "229" $ do
    expectOutput
      "true"
      "test/Coal/examples/229"
      [ "Main.coal"
      ]

  describe "230" $ do
    expectOutput
      "97"
      "test/Coal/examples/230"
      [ "Main.coal"
      ]

  describe "231" $ do
    expectOutput
      "true"
      "test/Coal/examples/231"
      [ "Main.coal"
      ]

  describe "232" $ do
    expectOutput
      "4"
      "test/Coal/examples/232"
      [ "Main.coal"
      ]

  describe "234" $ do
    expectOutput
      "b2"
      "test/Coal/examples/234"
      [ "Main.coal"
      ]

  describe "235" $ do
    expectOutput
      "0"
      "test/Coal/examples/235"
      [ "Main.coal"
      ]

  describe "235" $ do
    it "is PreflightFailure" $ do
      -- No module Main given
      res <- runSpec "test/Coal/examples/214" []
      res `shouldBe` Left PreflightFailure

  describe "236" $ do
    expectOutput
      "hello"
      "test/Coal/examples/236"
      [ "Main.coal"
      ]

  describe "237" $ do
    expectOutput
      "wat"
      "test/Coal/examples/237"
      [ "Main.coal"
      ]

  describe "238" $ do
    expectOutput
      "yup"
      "test/Coal/examples/238"
      [ "Main.coal"
      ]

  describe "239" $ do
    expectOutput
      "wat"
      "test/Coal/examples/239"
      [ "Main.coal"
      ]

  describe "240" $ do
    expectOutput
      "5"
      "test/Coal/examples/240"
      [ "Main.coal"
      ]

  describe "241" $ do
    expectOutput
      "1"
      "test/Coal/examples/241"
      [ "Main.coal"
      ]

  describe "246" $ do
    expectOutput
      "true,false"
      "test/Coal/examples/246"
      [ "Main.coal"
      ]

  describe "247" $ do
    expectOutput
      "true;false"
      "test/Coal/examples/247"
      [ "Main.coal"
      ]

  describe "248" $ do
    expectOutput
      "22"
      "test/Coal/examples/248"
      [ "Main.coal"
      ]

  describe "249" $ do
    expectOutput
      "3"
      "test/Coal/examples/249"
      [ "Main.coal"
      , "Containers/Map.coal"
      ]

  describe "250" $ do
    it "is NoSuchIdentifier" $ do
      res <- runSpec "test/Coal/examples/250" ["Main.coal"]
      res `shouldBe` Left NoSuchIdentifier

  describe "252" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec "test/Coal/examples/252" ["Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "253" $ do
    expectOutput
      "6"
      "test/Coal/examples/253"
      [ "Main.coal"
      ]

  describe "254" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/254" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "255" $ do
    expectOutput
      "2"
      "test/Coal/examples/255"
      [ "Main.coal"
      ]

  describe "257" $ do
    expectOutput
      "true"
      "test/Coal/examples/257"
      [ "Main.coal"
      ]

  describe "258" $ do
    expectOutput
      "1"
      "test/Coal/examples/258"
      [ "Main.coal"
      , "Containers/Set.coal"
      , "Containers/Map.coal"
      ]

  describe "259" $ do
    expectOutput
      "LT\nGT\nEQ"
      "test/Coal/examples/259"
      [ "Main.coal"
      ]

  describe "260" $ do
    expectOutput
      "4"
      "test/Coal/examples/260"
      [ "Main.coal"
      , "Containers/NonEmpty/List.coal"
      ]

  describe "261" $ do
    expectOutput
      "123"
      "test/Coal/examples/261"
      [ "Main.coal"
      , "Containers/Set.coal"
      , "Containers/Map.coal"
      ]

  describe "262" $ do
    expectOutput
      "4"
      "test/Coal/examples/262"
      [ "Main.coal"
      , "Containers/Tree.coal"
      ]

  describe "263" $ do
    expectOutput
      "2"
      "test/Coal/examples/263"
      [ "Main.coal"
      ]

  describe "264" $ do
    expectOutput
      "3"
      "test/Coal/examples/264"
      [ "Main.coal"
      ]

  describe "265" $ do
    expectOutput
      "4"
      "test/Coal/examples/265"
      [ "Main.coal"
      ]

  describe "266" $ do
    expectOutput
      "5"
      "test/Coal/examples/266"
      [ "Main.coal"
      ]

  describe "267" $ do
    expectOutput
      "123"
      "test/Coal/examples/267"
      [ "Main.coal"
      ]

  describe "270" $ do
    expectOutput
      "2"
      "test/Coal/examples/270"
      [ "Main.coal"
      , "Containers/Map.coal"
      , "Containers/Set.coal"
      ]

  describe "271" $ do
    expectOutput
      "false"
      "test/Coal/examples/271"
      [ "Main.coal"
      ]

  describe "272" $ do
    expectOutput
      "true"
      "test/Coal/examples/272"
      [ "Main.coal"
      ]

  describe "273" $ do
    expectOutput
      "!"
      "test/Coal/examples/273"
      [ "Main.coal"
      ]

  describe "274" $ do
    it "is NoSuchIdentifier" $ do
      res <- runSpec "test/Coal/examples/274" ["Main.coal"]
      res `shouldBe` Left NoSuchIdentifier

  describe "275" $ do
    expectOutput
      "8"
      "test/Coal/examples/275"
      [ "Main.coal"
      ]

  describe "276" $ do
    expectOutput
      "2"
      "test/Coal/examples/276"
      [ "Main.coal"
      ]

  describe "283" $ do
    expectOutput
      "3"
      "test/Coal/examples/283"
      [ "Main.coal"
      ]

  describe "284" $ do
    expectOutput
      "true"
      "test/Coal/examples/284"
      [ "Main.coal"
      ]

  describe "285" $ do
    expectOutput
      "hello,bob"
      "test/Coal/examples/285"
      [ "Main.coal"
      ]

  describe "287" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/287" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  --  describe "289" $ do
  --    it "is PreflightFailure" $ do
  --      res <- runSpec "test/Coal/examples/289" ["Main.coal"]
  --      res `shouldBe` Left PreflightFailure

  describe "290" $ do
    expectOutput
      "x"
      "test/Coal/examples/290"
      [ "Main.coal"
      ]

  describe "291" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/291" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "293" $ do
    expectOutput
      "hell,o"
      "test/Coal/examples/293"
      [ "Main.coal"
      , "Containers/Set.coal"
      , "Containers/Map.coal"
      ]

  describe "294" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/294" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "295" $ do
    it "is TypeError" $ do
      res <- runSpec "test/Coal/examples/295" ["Main.coal"]
      res `shouldBe` Left TypeError

  describe "296" $ do
    expectOutput
      "!"
      "test/Coal/examples/296"
      [ "Main.coal"
      ]

  describe "298" $ do
    expectOutput
      "!"
      "test/Coal/examples/298"
      [ "Main.coal"
      ]

  describe "299" $ do
    expectOutput
      "6"
      "test/Coal/examples/299"
      [ "Main.coal"
      ]

  describe "300" $ do
    expectOutput
      "1"
      "test/Coal/examples/300"
      [ "Main.coal"
      , "Containers/Set.coal"
      , "Containers/Map.coal"
      ]

  describe "301" $ do
    expectOutput
      "1"
      "test/Coal/examples/301"
      [ "Main.coal"
      , "Containers/Set.coal"
      , "Containers/Map.coal"
      ]

  describe "302" $ do
    expectOutput
      "5"
      "test/Coal/examples/302"
      [ "Main.coal"
      , "Containers/Set.coal"
      , "Containers/Map.coal"
      ]

  describe "303" $ do
    expectOutput
      "!"
      "test/Coal/examples/303"
      [ "Main.coal"
      , "Stuff.coal"
      ]

  describe "304" $ do
    expectOutput
      "✓ All 2 tests passed"
      "test/Coal/examples/304"
      [ "Main.coal"
      , "Test.coal"
      ]

  describe "306" $ do
    expectOutput
      "one\ntwo\nthree"
      "test/Coal/examples/306"
      [ "Main.coal"
      , "Writer.coal"
      ]

  describe "310" $ do
    expectOutput
      "hello\none\ntwo"
      "test/Coal/examples/310"
      [ "Main.coal"
      ]

  describe "311" $ do
    expectOutput
      "hello\none\ntwo"
      "test/Coal/examples/310"
      [ "Main.coal"
      ]

  describe "311" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/311" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "312" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/312" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "313" $ do
    it "is PreflightFailure" $ do
      res <- runSpec "test/Coal/examples/313" ["Main.coal"]
      res `shouldBe` Left PreflightFailure

expectOutput :: String -> String -> [FilePath] -> Spec
expectOutput expt srcPath files =
  it ("\"" <> expt <> "\"") $ do
    res <- runSpec srcPath files
    res `shouldBe` Right (expt <> "\n")

runSpec :: FilePath -> [FilePath] -> IO (Either CompilerFailureMode String)
runSpec srcPath files = do
  e <-
    evalCompilerT (emptyCompilerEnvironment Nothing) $ do
      -- TODO: cache?
      setConfigC defaultConfig{configNoCache = True, configSilent = True, configSourcePaths = [srcPath]}
      runPass pipeline files
  case e of
    Left e1 ->
      pure (Left e1)
    Right{} -> do
      txt <- readProcess "./dist" [] ""
      pure (Right txt)
