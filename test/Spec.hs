{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Free
import CLI.Command.BuildSpec (buildSpec)
import CLI.Parser.AddCmdSpec (addCmdSpec)
import CLI.Parser.CommandSpec (commandSpec)
import CLI.Parser.UpdateCmdSpec (updateCmdSpec)
import Coal.Compiler.ConfigSpec (configHashSpec)
import Coal.Compiler.Pass.PhaseParsing.ParsingSpec (parsingSpec)
import Coal.Compiler.Pass.PhasePreflight.ExpandLetBindingsSpec (expandLetBindingsSpec)
import Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec)
import Coal.Kernel.Spec (kernelSpec)
import Coal.Language.Module.ExportSpec (exportMatchingSpec)
import Coal.Language.PatternSpec (patternBoundVarsSpec)
import Coal.Language.TypeSpec (typeApplicationSpec, typeArgsSpec)
import Coal.Parser.DoNotationSpec (doNotationSpec)
import Coal.Parser.ExpressionSpec (expressionSpec)
import Coal.Parser.ModuleSpec (moduleHeaderSpec)
import Coal.Parser.PatternSpec (patternSpec)
import Coal.TypeSystemSpec (typeSystemSpec)
import E2E.Spec (e2eSpec)
import Control.Monad.State
import Package.ConstraintValidationSpec (constraintValidationSpec)
import Package.ManifestSpec (manifestSpec)
import Package.ResolutionSpec (resolutionSpec)
import Package.VersionSpec (versionSpec)
import Test.Hspec (SpecWith, describe, hspec)
import Control.Monad.Writer

spec :: SpecWith ()
spec =
  describe "Unit tests" $ do
    configHashSpec
    parsingSpec
    typeSystemSpec
    typeArgsSpec
    typeApplicationSpec
    patternAnomaliesSpec
    expandLetBindingsSpec
    doNotationSpec
    expressionSpec
    moduleHeaderSpec
    patternSpec
    patternBoundVarsSpec
    exportMatchingSpec

main :: IO ()
main =
  hspec $ do
    spec
    describe "CLI tests" $ do
      addCmdSpec
      commandSpec
      buildSpec
      constraintValidationSpec
      manifestSpec
      resolutionSpec
      versionSpec
      updateCmdSpec

    describe "Kernel tests" kernelSpec
    describe "E2E tests" e2eSpec


traverseList :: [Int] -> Writer [Int] ()
traverseList xs =
  case xs of
    y : ys -> do
      tell [y]
      traverseList ys
    [] -> 
      pure ()

data ListPair = Pair [Int] [Int]


baz :: ListPair -> Writer [Int] ()
baz p = do
  case p of
    Pair p1 p2 -> do
      let a = execWriter $ traverseList p1
      let b = execWriter $ traverseList p2
      tell a
      tell b

foo3 =
  execWriter (baz (Pair [1,2] [3,4]))

-----------------------------


yield val =
  modify $ \(f, s) -> (f, f val s)

zigZagLists xs ys =
  case xs of
    [] ->
      pure ()
    xhd : xtl ->
      case ys of
        [] ->
          pure ()
        yhd : ytl -> do
          yield xhd
          yield yhd
          zigZagLists xtl ytl

zip_ p =
  case p of
    Pair p1 p2 -> do
      zigZagLists p1 p2

walk f1 x f2 y = 
  snd $ execState (f1 x) (f2, y)

run =
  walk 
    zip_ 
    (Pair [1,2] [3,4])
    (\x s -> s ++ [x]) 
    []
    

----

part xs =
  case xs of
      [] -> error "empty list" 
      y : ys ->
        (y : filter (== y) ys, filter (< y) ys, filter (> y) ys)

    
bisect :: [Int] -> State ([Int] -> t -> t, t) ()
bisect xs =
  case xs of
    [] ->
      pure ()
    _ -> do
      let (p, lhs, rhs) = part xs
      bisect lhs
      yield p
      bisect rhs


run2 =
  walk 
    bisect
    [4, 5, 2, 1, 7, 3, 6]
    (\x s -> s ++ x) 
    []
    

---

data ArithmeticF a
  = Add Int Int (Int -> a)
  | Mul Int Int (Int -> a)
  | Sub Int Int (Int -> a)
  deriving (Functor)


type Arithm = Free ArithmeticF

instrScheme instr = do
  case instr of
    Add a b next -> do
      yield $ next (a + b)
      freeScheme $ next (a + b)
    _ ->
      pure undefined

freeScheme fr = do
  case fr of
    Pure x ->
      undefined -- yield x
    Free f ->
      instrScheme f


prog :: Arithm Int
prog = do
  x <- liftF (Add 5 3 id)
  liftF (Add x 3 id)

run3 =
  walk 
    freeScheme
    prog
    (\x s -> undefined) 
    0
    

