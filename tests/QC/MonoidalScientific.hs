{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, TypeApplications, DataKinds, DeriveGeneric #-}

module QC.MonoidalScientific where

import Data.Scientific (scientific)
import GHC.Generics
import QC.Common (Repack, parseBS, parseLbsBack, repackBS)
import Test.Tasty (TestTree)
import Test.QuickCheck
import Test.Tasty.QuickCheck (testProperty)
import Debug.TraceEmbrace
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B8

monoidalDecimal :: Word -> Repack -> Bool
monoidalDecimal n rs =
  $(tw "/n rs parsed parsedBack") $ parsed == parsedBack
  where
    parsedBack = parseLbsBack P.decimal input
    parsed :: Maybe Int = parseBS P.decimal input
    input = repackBS rs $ B8.pack (show n)

monoidalDouble :: Double -> Repack -> Bool
monoidalDouble d rs =
  $(tw "/d rs parsed parsedBack") $ parsed == parsedBack
  where
    parsedBack = parseLbsBack P.double input
    parsed = parseBS P.double input
    input = repackBS rs $ B8.pack (show d)

monoidalDouble' :: Double -> Repack -> Bool
monoidalDouble' d rs =
  $(tw "/d rs parsed parsed'") $ parsed == parsed'
  where
    parsed' = parseBS P.double' input
    parsed = parseBS P.double input
    input = repackBS rs $ B8.pack (show d)

data Digit
  = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show, Eq, Enum, Bounded, Generic)

instance Arbitrary Digit where
  arbitrary = elements (enumFromTo minBound maxBound)
  shrink = genericShrink

monoidalScientific' :: Int -> Bool -> Digit -> Repack -> Bool
monoidalScientific' s si d rs =
  $(tw "/d rs parsed parsed'") $ parsed == parsed'
  where
    parsed' = parseBS P.scientific' input
    parsed = parseBS P.scientific input
    input = repackBS rs $ B8.pack (show $ scientific (fromIntegral s) (si' * fromEnum d))
    si' = if si then 1 else -1

tests :: [TestTree]
tests = [
    testProperty "decimal" monoidalDecimal
  , testProperty "double" monoidalDouble
  , testProperty "doubleConsistent" monoidalDouble'
  , testProperty "scientificConsistent" monoidalScientific'
  ]
