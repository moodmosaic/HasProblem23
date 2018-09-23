{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
  ) where

import           Control.Monad  (unless)
import           Data.List      (genericLength)
import           Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Gen.QuickCheck as Gen
import qualified Hedgehog.Range as Range
import           Lib
import           System.Exit    (exitFailure)
import           System.Random

lenProp g = property $ do
  seed <- forAll $ Gen.int Range.linearBounded
  xs   <- forAll $ Gen.list (Range.linear 0 100) g
  i    <- forAll $ Gen.int $ Range.linear 0 100
  i === genericLength (rndGenSelect (mkStdGen seed) xs i)

negLenProp g = property $ do
  seed <- forAll $ Gen.int Range.linearBounded
  xs   <- forAll $ Gen.list (Range.linear 0 100) g
  i    <- forAll $ Gen.int $ Range.linear 1 100
  0 === genericLength (rndGenSelect (mkStdGen seed) xs (-i))

main :: IO ()
main = do
  results <- sequence [
      properties
    , regressionTests
    ]

  unless (and results)
    exitFailure

properties :: IO Bool
properties =
  checkParallel $ Group "Properties" [
        ("rndGenSelect returns result of correct length"
        , lenProp (Gen.arbitrary :: Gen Int))
      , ("rndGenSelect returns result of correct length"
        , lenProp (Gen.arbitrary :: Gen Char))
      , ("rndGenSelect returns empty result when count is negative"
        , negLenProp (Gen.arbitrary :: Gen Int))
    ]

regressionTests :: IO Bool
regressionTests =
  checkParallel $ Group "Regression tests" $
    do
    (seed, xs, count, expected) <-
      [
        (     42,      "foo",  3,        "ofo"),
        (   1337,      "bar", 10, "rabbaarrra"),
        (-197221, ['a'..'z'],  5,      "ntfnc")
      ]
    let rnd = mkStdGen seed

    let actual = rndGenSelect rnd xs count

    return
      ("rndGenSelect of chars returns correct result"
      , withTests 1 . property $ expected === actual)

    ++

    do
    (seed, xs, count, expected) <-
      [
        (  19,      [1..3],  3,               [3,1,3]),
        (1770, [0,1,1,2,7], 10, [1,2,2,1,1,1,1,1,7,7]),
        ( -19,     [0..99],  5,       [67,48,8,47,42])
      ]
    let rnd = mkStdGen seed

    let actual = rndGenSelect rnd xs count

    return
      ("rndGenSelect of integers returns correct result"
      , withTests 1 . property $ expected === actual)
