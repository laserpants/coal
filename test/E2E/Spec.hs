{-# LANGUAGE OverloadedStrings #-}

module E2E.Spec (e2eSpec, runSpec) where

import Coal.Compiler (pipeline)
import Coal.Compiler.Environment
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import System.Process
import Test.Hspec

e2eSpec :: Spec
e2eSpec = do
  describe "001" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/001/Main.coal"]
      res `shouldBe` Left TypeError

  describe "002" $
    expectOutput
      "24"
      [ "./test/Coal/examples/002/Main.coal"
      , "./lang/Nat.coal"
      ]

  describe "003" $
    expectOutput
      "1"
      [ "./test/Coal/examples/003/Main.coal"
      , "./lang/Coal/Combinators.coal"
      ]

  describe "004" $
    expectOutput
      "2"
      [ "./test/Coal/examples/004/Main.coal"
      , "./lang/Coal/Combinators.coal"
      ]

  describe "005" $
    expectOutput
      "40320"
      [ "./test/Coal/examples/005/Math.coal"
      , "./test/Coal/examples/005/Main.coal"
      , "./lang/Nat.coal"
      ]

  describe "006" $
    expectOutput
      "101"
      [ "./test/Coal/examples/006/Tree.coal"
      , "./test/Coal/examples/006/Qsort.coal"
      , "./test/Coal/examples/006/Main.coal"
      , "./lang/Coal/Combinators.coal"
      ]

  describe "007" $
    expectOutput
      "hello"
      [ "./test/Coal/examples/007/Main.coal"
      ]

  describe "008" $
    expectOutput
      "cluedo"
      [ "./test/Coal/examples/008/Main.coal"
      ]

  describe "009" $
    expectOutput
      "wat"
      [ "./test/Coal/examples/009/Main.coal"
      ]

  describe "010" $
    expectOutput "hello from the other side" ["./test/Coal/examples/010/Main.coal"]

  describe "011" $
    expectOutput "Covfefe" ["./test/Coal/examples/011/Main.coal"]

  describe "012" $
    expectOutput "bork bork bork" ["./test/Coal/examples/012/Main.coal"]

  describe "013" $ do
    it "is TypeError" $ do
      res <-
        runSpec
          [ "./test/Coal/examples/013/Main.coal"
          , "./lang/Nat.coal"
          ]
      res `shouldBe` Left TypeError

  describe "014" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/014/Main.coal"]
      res `shouldBe` Left TypeError

  describe "015" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/015/Main.coal"]
      res `shouldBe` Left TypeError

  describe "016" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/016/Main.coal"]
      res `shouldBe` Left TypeError

  describe "017" $
    expectOutput "false" ["./test/Coal/examples/017/Main.coal"]

  describe "018" $
    expectOutput
      "40320"
      [ "./test/Coal/examples/018/Main.coal"
      , "./lang/Nat.coal"
      ]

  describe "019" $
    expectOutput "Bob" ["./test/Coal/examples/019/Main.coal"]

  describe "020" $
    expectOutput "Lazarus" ["./test/Coal/examples/020/Main.coal"]

  describe "021" $
    expectOutput "Alphonso" ["./test/Coal/examples/021/Main.coal"]

  describe "022" $
    expectOutput
      "1234"
      [ "./test/Coal/examples/022/Main.coal"
      , "./lang/String.coal"
      ]

  describe "023" $
    expectOutput
      "123"
      [ "./test/Coal/examples/023/Main.coal"
      , "./lang/String.coal"
      ]

  describe "024" $
    expectOutput
      "x"
      [ "./test/Coal/examples/024/Main.coal"
      , "./lang/String.coal"
      ]

  describe "025" $ do
    it "is NoSuchIdentifier" $ do
      res <- runSpec ["./test/Coal/examples/025/Main.coal"]
      res `shouldBe` Left NoSuchIdentifier

  describe "026" $
    expectOutput "5" ["./test/Coal/examples/026/Main.coal"]

  describe "027" $
    expectOutput
      "ananab"
      [ "./test/Coal/examples/027/Main.coal"
      , "./lang/String.coal"
      ]

  describe "028" $
    expectOutput "2" ["./test/Coal/examples/028/Main.coal"]

  describe "029" $
    expectOutput "5" ["./test/Coal/examples/029/Main.coal"]

  describe "030" $
    expectOutput
      "123"
      [ "./test/Coal/examples/030/Main.coal"
      , "./lang/String.coal"
      ]

  describe "031" $
    expectOutput
      "111\n111"
      [ "./test/Coal/examples/031/Main.coal"
      , "./lang/String.coal"
      ]

  describe "032" $
    expectOutput
      "9876"
      [ "./test/Coal/examples/032/Main.coal"
      , "./lang/String.coal"
      ]

  describe "033" $
    expectOutput
      "-123"
      [ "./test/Coal/examples/033/Main.coal"
      , "./lang/String.coal"
      ]

  describe "034" $
    expectOutput
      "59876"
      [ "./test/Coal/examples/034/Main.coal"
      , "./lang/String.coal"
      ]

  describe "035" $
    expectOutput "true" ["./test/Coal/examples/035/Main.coal"]

  describe "036" $
    expectOutput "true" ["./test/Coal/examples/036/Main.coal"]

  describe "037" $
    expectOutput "true" ["./test/Coal/examples/037/Main.coal"]

  describe "038" $
    expectOutput "true" ["./test/Coal/examples/038/Main.coal"]

  describe "039" $
    expectOutput "2" ["./test/Coal/examples/039/Main.coal"]

  describe "040" $
    expectOutput "6" ["./test/Coal/examples/040/Main.coal"]

  describe "041" $
    expectOutput "512" ["./test/Coal/examples/041/Main.coal"]

  describe "042" $
    expectOutput "8" ["./test/Coal/examples/042/Main.coal"]

  describe "043" $
    expectOutput "1" ["./test/Coal/examples/043/Main.coal"]

  describe "044" $
    expectOutput "false" ["./test/Coal/examples/044/Main.coal"]

  describe "045" $
    expectOutput "1" ["./test/Coal/examples/045/Main.coal"]

  describe "046" $
    expectOutput "22.500000" ["./test/Coal/examples/046/Main.coal"]

  describe "047" $
    expectOutput "23.000000000000000" ["./test/Coal/examples/047/Main.coal"]

  describe "048" $
    expectOutput "123" ["./test/Coal/examples/048/Main.coal"]

  describe "049" $ do
    it "is NoSuchIdentifier" $ do
      res <- runSpec ["./test/Coal/examples/049/Main.coal"]
      res `shouldBe` Left NoSuchIdentifier

  describe "050" $
    expectOutput
      "720"
      [ "./test/Coal/examples/050/Main.coal"
      , "./lang/Nat.coal"
      ]

  describe "051" $
    expectOutput "Prot" ["./test/Coal/examples/051/Main.coal"]

  describe "052" $
    expectOutput "prot" ["./test/Coal/examples/052/Main.coal"]

  describe "053" $
    expectOutput "Wat" ["./test/Coal/examples/053/Main.coal"]

  describe "054" $
    expectOutput "hello world" ["./test/Coal/examples/054/Main.coal"]

  describe "055" $
    expectOutput "Covfefe" ["./test/Coal/examples/055/Main.coal"]

  describe "056" $
    expectOutput "true" ["./test/Coal/examples/056/Main.coal"]

  describe "057" $
    expectOutput "Lorenzo" ["./test/Coal/examples/057/Main.coal"]

  describe "058" $
    expectOutput "Lorenzo" ["./test/Coal/examples/058/Main.coal"]

  describe "059" $
    expectOutput "a" ["./test/Coal/examples/059/Main.coal"]

  describe "060" $
    expectOutput "true" ["./test/Coal/examples/060/Main.coal"]

  describe "061" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/061/Main.coal"]
      res `shouldBe` Left TypeError

  describe "062" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/062/Main.coal"]
      res `shouldBe` Left TypeError

  describe "063" $
    expectOutput
      "24"
      [ "./test/Coal/examples/063/Main.coal"
      , "./lang/Nat.coal"
      ]

  describe "064" $
    expectOutput "6" ["./test/Coal/examples/064/Main.coal"]

  describe "065" $
    expectOutput "true" ["./test/Coal/examples/065/Main.coal"]

  describe "066" $
    expectOutput
      "7"
      [ "./lang/Coal/Combinators.coal"
      , "./test/Coal/examples/066/List.coal"
      , "./test/Coal/examples/066/Main.coal"
      ]

  describe "067" $
    expectOutput
      "{\"abc\":[\"a\",\"b\",\"c\"],\"pi\":3.14159}"
      [ "./test/Coal/examples/067/List.coal"
      , "./test/Coal/examples/067/StringUtils.coal"
      , "./test/Coal/examples/067/Main.coal"
      , "./lang/String.coal"
      ]

  describe "068" $
    expectOutput "3" ["./test/Coal/examples/068/Main.coal"]

  describe "069" $
    expectOutput "512" ["./test/Coal/examples/069/Main.coal"]

  describe "070" $
    expectOutput "2" ["./test/Coal/examples/070/Main.coal"]

  describe "071" $
    expectOutput
      "100"
      [ "./test/Coal/examples/071/Main.coal"
      , "./lang/Nat.coal"
      ]

  describe "072" $
    expectOutput "2" ["./test/Coal/examples/072/Main.coal"]

  describe "073" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/073/Main.coal"]
      res `shouldBe` Left TypeError

  describe "074" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/074/Main.coal"]
      res `shouldBe` Left TypeError

  describe "075" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec ["./test/Coal/examples/075/Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "076" $
    expectOutput "1" ["./test/Coal/examples/076/Main.coal"]

  describe "077" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec ["./test/Coal/examples/077/Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "078" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec ["./test/Coal/examples/078/Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "079" $
    expectOutput "5" ["./test/Coal/examples/079/Main.coal"]

  describe "080" $ do
    it "is TypeError" $ do
      res <- runSpec ["./test/Coal/examples/080/Main.coal"]
      res `shouldBe` Left TypeError

  describe "081" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec ["./test/Coal/examples/081/Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "082" $ do
    expectOutput "3" ["./test/Coal/examples/082/Main.coal"]

  describe "083" $ do
    expectOutput "4" ["./test/Coal/examples/083/Main.coal"]

  describe "084" $ do
    expectOutput "false" ["./test/Coal/examples/084/Main.coal"]

  describe "085" $ do
    expectOutput "-5" ["./test/Coal/examples/085/Main.coal"]

  describe "086" $ do
    expectOutput "1" ["./test/Coal/examples/086/Main.coal"]

  describe "087" $ do
    expectOutput "4" ["./test/Coal/examples/087/Main.coal"]

  describe "088" $ do
    expectOutput "5" ["./test/Coal/examples/088/Main.coal"]

  describe "089" $ do
    expectOutput "3" ["./test/Coal/examples/089/Main.coal"]

  describe "090" $ do
    expectOutput "3.000000000000000" ["./test/Coal/examples/090/Main.coal"]

  describe "091" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec ["./test/Coal/examples/091/Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "092" $ do
    expectOutput "123" ["./test/Coal/examples/092/Main.coal"]

  describe "093" $ do
    expectOutput "321" ["./test/Coal/examples/093/Main.coal"]

  describe "094" $ do
    it "is PatternAnomaly" $ do
      res <- runSpec ["./test/Coal/examples/094/Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "095" $ do
    expectOutput "3" ["./test/Coal/examples/095/Main.coal"]

  describe "096" $ do
    expectOutput "Hello Space" ["./test/Coal/examples/096/Main.coal"]

  describe "097" $ do
    expectOutput "2" ["./test/Coal/examples/097/Main.coal"]

  describe "098" $ do
    expectOutput "-627128164" ["./test/Coal/examples/098/Main.coal"]

  describe "099" $ do
    expectOutput
      "8"
      [ "./test/Coal/examples/099/Main.coal"
      , "./lang/Nat.coal"
      ]

  describe "100" $ do
    expectOutput "4.323232444322323" ["./test/Coal/examples/100/Main.coal"]

  describe "101" $ do
    expectOutput "4.130000" ["./test/Coal/examples/101/Main.coal"]

  describe "102" $ do
    expectOutput "3" ["./test/Coal/examples/102/Main.coal"]

  describe "103" $ do
    expectOutput "1" ["./test/Coal/examples/103/Main.coal"]

  describe "104" $ do
    expectOutput "6" ["./test/Coal/examples/104/Main.coal"]

  describe "105" $ do
    it "is TraitError" $ do
      res <- runSpec ["./test/Coal/examples/105/Main.coal"]
      res `shouldBe` Left TraitError

  describe "106" $ do
    it "is PreflightFailure" $ do
      res <- runSpec ["./test/Coal/examples/106/Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "107" $ do
    it "is PreflightFailure" $ do
      res <- runSpec ["./test/Coal/examples/107/Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "108" $ do
    it "is MissingMainEntryPoint" $ do
      res <- runSpec ["./test/Coal/examples/108/Main.coal"]
      res `shouldBe` Left MissingMainEntryPoint

  describe "109" $ do
    expectOutput "Hello, world!" ["./test/Coal/examples/109/Main.coal"]

  describe "110" $ do
    expectOutput "d" ["./test/Coal/examples/110/Main.coal"]

  describe "111" $ do
    expectOutput "b" ["./test/Coal/examples/111/Main.coal"]

  describe "112" $ do
    expectOutput "5" ["./test/Coal/examples/112/Main.coal"]

  describe "113" $ do
    expectOutput "a" ["./test/Coal/examples/113/Main.coal"]

  describe "114" $ do
    it "is PreflightFailure" $ do
      res <- runSpec ["./test/Coal/examples/114/Main.coal"]
      res `shouldBe` Left PreflightFailure

  describe "115" $ do
    expectOutput "b" ["./test/Coal/examples/115/Main.coal"]

  describe "116" $ do
    it "is PreflightFailure" $ do
      res <- runSpec ["./test/Coal/examples/116/Main.coal"]
      res `shouldBe` Left PatternAnomaly

  describe "117" $ do
    expectOutput "4" ["./test/Coal/examples/117/Main.coal"]

  describe "118" $ do
    expectOutput "hellohello" ["./test/Coal/examples/118/Main.coal"]

  describe "119" $ do
    expectOutput "hello" ["./test/Coal/examples/119/Main.coal"]

  describe "120" $ do
    expectOutput "true" ["./test/Coal/examples/120/Main.coal"]

  describe "121" $ do
    expectOutput
      "342"
      [ "./test/Coal/examples/121/Main.coal"
      , "./lang/String.coal"
      ]

  describe "122" $ do
    it "is NoSuchIdentifier" $ do
      res <-
        runSpec
          [ "./test/Coal/examples/122/Main.coal"
          , "./lang/String.coal"
          ]
      res `shouldBe` Left NoSuchIdentifier

  describe "123" $ do
    expectOutput
      "4.1"
      [ "./test/Coal/examples/123/Main.coal"
      , "./lang/String.coal"
      ]

  describe "124" $ do
    expectOutput
      "ail"
      [ "./test/Coal/examples/124/Main.coal"
      , "./lang/String.coal"
      ]

  describe "125" $ do
    expectOutput
      "11"
      [ "./test/Coal/examples/125/Main.coal"
      , "./lang/String.coal"
      ]

  describe "126" $ do
    expectOutput
      "h"
      [ "./test/Coal/examples/126/Main.coal"
      , "./lang/String.coal"
      ]

  describe "127" $ do
    it "is PreflightFailure" $ do
      res <-
        runSpec
          [ "./test/Coal/examples/127/Main.coal"
          , "./test/Coal/examples/127/Foo.coal"
          , "./lang/String.coal"
          ]
      res `shouldBe` Left PreflightFailure

  describe "128" $ do
    it "is PreflightFailure" $ do
      res <-
        runSpec
          [ "./test/Coal/examples/128/Main.coal"
          , "./lang/String.coal"
          ]
      res `shouldBe` Left PreflightFailure

  describe "129" $ do
    expectOutput
      "Hello, World!\n"
      [ "./test/Coal/examples/129/Main.coal"
      , "./lang/String.coal"
      ]

  describe "131" $ do
    expectOutput
      "🚀"
      [ "./test/Coal/examples/131/Main.coal"
      , "./lang/String.coal"
      ]

  describe "136" $
    expectOutput
      "101"
      [ "./test/Coal/examples/136/Tree.coal"
      , "./test/Coal/examples/136/Qsort.coal"
      , "./test/Coal/examples/136/Main.coal"
      , "./lang/Coal/Combinators.coal"
      ]

  describe "137" $
    expectOutput
      "2"
      [ "./test/Coal/examples/137/Eq.coal"
      , "./test/Coal/examples/137/Stuff.coal"
      , "./test/Coal/examples/137/Main.coal"
      , "./lang/Coal/Combinators.coal"
      ]

  describe "138" $
    expectOutput
      "3"
      [ "./test/Coal/examples/138/Eq.coal"
      , "./test/Coal/examples/138/Stuff.coal"
      , "./test/Coal/examples/138/Main.coal"
      , "./lang/Coal/Combinators.coal"
      ]

  describe "139" $ do
    it "is TraitError" $ do
      res <-
        runSpec
          [ "./test/Coal/examples/139/Eq.coal"
          , "./test/Coal/examples/139/Stuff.coal"
          , "./test/Coal/examples/139/Main.coal"
          , "./lang/Coal/Combinators.coal"
          ]
      res `shouldBe` Left TraitError

  describe "140" $ do
    it "is TypeError" $ do
      res <-
        runSpec
          [ "./test/Coal/examples/140/Eq.coal"
          , "./test/Coal/examples/140/Stuff.coal"
          , "./test/Coal/examples/140/Main.coal"
          , "./lang/Coal/Combinators.coal"
          ]
      res `shouldBe` Left TypeError

expectOutput :: String -> [FilePath] -> Spec
expectOutput expt files =
  it ("\"" <> expt <> "\"") $ do
    res <- runSpec files
    res `shouldBe` Right (expt <> "\n")

runSpec :: [FilePath] -> IO (Either CompilerFailureMode String)
runSpec files = do
  (e, _, _) <-
    runCompilerT emptyCompilerEnvironment $
      runPass pipeline (files <> ["./lang/IO.coal"])
  case e of
    Left e1 ->
      pure (Left e1)
    Right{} -> do
      txt <- readProcess "./dist" [] ""
      pure (Right txt)
