{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell, ScopedTypeVariables, BangPatterns, TypeApplications, DataKinds #-}

module QC.CombinatorRight where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>), (<*), (*>))
#endif
import Control.Applicative ((<|>))
import Data.Maybe (fromJust, isJust)
import Data.Word (Word8)
import QC.Common (Repack, parseLbsBack, repackBS, toLazyBS)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Debug.TraceEmbrace
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.Attoparsec.Combinator as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec.ByteString ((>*<))

choice :: NonEmptyList (NonEmptyList Word8) -> Gen Property
choice (NonEmpty xs) = do
  let ys = map (B.pack . getNonEmpty) xs
  return . forAll (repackBS <$> arbitrary <*> elements ys) $
    maybe False (`elem` ys) . parseLbsBack (C.choice (map P.string ys))

count :: Positive (Small Int) -> Repack -> B.ByteString -> Bool
count (Positive (Small n)) rs s =
    (length <$> parseLbsBack (C.count n (P.string s)) input) == Just n
  where input = repackBS rs (B.concat (replicate (n+1) s))

lookAhead :: NonEmptyList Word8 -> Bool
lookAhead (NonEmpty xs) =
  let ys = B.pack xs
      withLookAheadThenConsume = (\x y -> (x, y)) <$> C.lookAhead (P.string ys) <*> P.string ys
      mr = parseLbsBack withLookAheadThenConsume $ toLazyBS ys
  in isJust mr && fst (fromJust mr) == snd (fromJust mr)

match :: Int -> NonNegative Int -> NonNegative Int -> Repack -> Bool
match n (NonNegative x) (NonNegative y) rs =
    $(tw "parsed/n x y rs") (parseLbsBack (P.match parser)
      ($(tw' "repacked/n x y rs") (repackBS rs input))) ==
        Just ($(tw' "input/n x y rs") input, n)
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          r <- $(tw "signed decimal/") <$> P.signed P.decimal
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c x/c") $ c =='x')
          pure $ $(tw "return sdec/") r
        input = B.concat [
            B8.replicate x 'x', B8.pack (show n), B8.replicate y 'y'
          ]

matchOp :: Int -> NonNegative Int -> NonNegative Int -> Repack -> Bool
matchOp n (NonNegative x) (NonNegative y) rs =
      parseLbsBack (P.match parser) (repackBS rs input) == Just ($(tw' "input") input, n)

  where parser = P.skipWhile (=='y') *> P.signed P.decimal <*
                 P.skipWhile (=='x')
        input = B.concat [
            B8.replicate x 'x', B8.pack (show n), B8.replicate y 'y'
          ]

skipWhileX :: NonNegative Int -> NonNegative Int -> Repack -> Bool
skipWhileX (NonNegative x) (NonNegative y) rs =
    $(tw "parsed/x y rs") (parseLbsBack (P.match parser)
      ($(tw' "repacked/x y rs") (repackBS rs input))) ==
        Just ($(tw' "input/x y rs") input, ())
  where parser = do
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          P.endOfInput
        input = B.concat [ B8.replicate x 'x', B8.replicate y 'x' ]

skipWhileY_X :: NonNegative Int -> NonNegative Int -> Repack -> Bool
skipWhileY_X (NonNegative x) (NonNegative y) rs =
    $(tw "parsed/x y rs") (parseLbsBack (P.match parser)
      ($(tw' "repacked/x y rs") (repackBS rs input))) ==
        Just ($(tw' "input/x y rs") input, 95)
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          ub <- $(tr "took _") <$> P.char8 '_'
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          P.endOfInput
          pure ub
        input = B.concat [ B8.replicate x 'x', B8.replicate 1 '_', B8.replicate y 'y' ]

skipWhile_Y_X :: NonNegative Int -> NonNegative Int -> Repack -> Bool
skipWhile_Y_X (NonNegative x) (NonNegative y) rs =
    $(tw "parsed/x y rs") (parseLbsBack (P.match parser)
      ($(tw' "repacked/x y rs") (repackBS rs input))) ==
        Just ($(tw' "input/x y rs") input, (95, 95))
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          ub <- $(tr "took _") <$> P.char8 '_'
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          ub2 <- $(tr "took _") <$> P.char8 '_'
          P.endOfInput
          pure (ub, ub2)
        input = B.concat [ B8.replicate 1 '_'
                         , B8.replicate y 'x'
                         , B8.replicate x 'x'
                         , B8.replicate 1 '_'
                         , B8.replicate x 'y'
                         , B8.replicate y 'y'
                         ]

skipWhileYNatX :: NonNegative Int -> NonNegative Int -> NonNegative Int -> Repack -> Bool
skipWhileYNatX (NonNegative n) (NonNegative x) (NonNegative y) rs =
    $(tw "parsed/n x y rs") (parseLbsBack (P.match parser)
      ($(tw' "repacked/n x y rs") (repackBS rs input))) ==
        Just ($(tw' "input/n x y rs") input, n)
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          ub <- $(tr "took decimal") <$> P.decimal
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          P.endOfInput
          pure ub
        input = B.concat [ B8.replicate x 'x', B8.pack $ show n, B8.replicate y 'y' ]

skipWhileYMinusNatX :: Negative Int -> NonNegative Int -> NonNegative Int -> Repack -> Bool
skipWhileYMinusNatX (Negative n) (NonNegative x) (NonNegative y) rs =
    $(tw "parsed/n x y rs") (parseLbsBack (P.match parser)
      ($(tw' "repacked/n x y rs") (repackBS rs input))) ==
        Just ($(tw' "input/n x y rs") input, n * (-1))
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          (_, ub) <- $(tr "took decimal") <$> (P.char8 '-' >*< P.decimal)
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          P.endOfInput
          pure ub
        input = B.concat [ B8.replicate x 'x', B8.pack $ show n, B8.replicate y 'y' ]

skipWhileYMinusTryNatXNoRepackWithoutMatch :: Negative Int -> NonNegative Int -> NonNegative Int -> Bool
skipWhileYMinusTryNatXNoRepackWithoutMatch (Negative n) (NonNegative x) (NonNegative y) =
    $(tw "parsed/n x y") (parseLbsBack parser -- (P.match parser)
      ($(tw' "repacked/n x y") (toLazyBS input))) ==
        Just {- ($(tw' "input/n x y rs") input,-} (n * (-1)) -- )
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          (_, ub) <- $(tr "took decimal") <$> (P.try (P.char8 '+' >*< P.decimal) <|> P.char8 '-' >*< P.decimal)
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          P.endOfInput
          pure ub
        input = B.concat [ B8.replicate x 'x', B8.pack $ show n, B8.replicate y 'y' ]

skipWhileYMinusTryNatXNoRepackWithMatch :: Negative Int -> NonNegative Int -> NonNegative Int -> Bool
skipWhileYMinusTryNatXNoRepackWithMatch (Negative n) (NonNegative x) (NonNegative y) =
    $(tw "parsed/n x y") (parseLbsBack (P.match parser)
      ($(tw' "repacked/n x y") (toLazyBS input))) ==
        Just ($(tw' "input/n x y") input, n * (-1))
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          (_, ub) <- $(tr "took decimal") <$> (P.char8 '+' >*< P.try P.decimal <|> P.char8 '-' >*< P.decimal)
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          P.endOfInput
          pure ub
        input = B.concat [ B8.replicate x 'x', B8.pack $ show n, B8.replicate y 'y' ]

skipWhileYMinusNatXNoRepackWithMatch :: Negative Int -> NonNegative Int -> NonNegative Int -> Bool
skipWhileYMinusNatXNoRepackWithMatch (Negative n) (NonNegative x) (NonNegative y) =
    $(tw "parsed/n x y") (parseLbsBack (P.match parser)
      ($(tw' "repacked/n x y") (toLazyBS input))) ==
        Just ($(tw' "input/n x y") input, n * (-1))
  where parser = do
          $(tr "skipped y") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='y')
          (_, ub) <- $(tr "took decimal") <$> (P.char8 '+' >*< P.decimal <|> P.char8 '-' >*< P.decimal)
          $(tr "skipped x") <$>
            P.skipWhile (\c -> $(tw "is c y/c") $ c =='x')
          P.endOfInput
          pure ub
        input = B.concat [ B8.replicate x 'x', B8.pack $ show n, B8.replicate y 'y' ]

backtrackWithDrift :: Bool
backtrackWithDrift = (parseLbsBack parser input) == Just (120, 121)
  where parser = do
          c2 <- (P.char8 'z' >*< P.char8 'y') <|> (P.char8 'x' >*< P.char8 'y')
          P.endOfInput
          pure c2
        input = toLazyBS (B8.pack "x") <> toLazyBS (B8.pack "y")

tests :: [TestTree]
tests = [
    testProperty "choice" choice
  , testProperty "count" count
  , testProperty "lookAhead" lookAhead
  , testProperty "match" match
  , testProperty "matchOp" matchOp
  , testProperty "skipWhileX" skipWhileX
  , testProperty "skipWhileY_X" skipWhileY_X
  , testProperty "skipWhile_Y_X" skipWhile_Y_X
  , testProperty "skipWhileYNatX" skipWhileYNatX
  , testProperty "skipWhileYMinusNatX" skipWhileYMinusNatX
  , testProperty "skipWhileYMinusTryNatXNoRepackWithoutMatch" skipWhileYMinusTryNatXNoRepackWithoutMatch
  , testProperty "skipWhileYMinusTryNatXNoRepackWithMatch" skipWhileYMinusTryNatXNoRepackWithMatch
  , testProperty "skipWhileYMinusNatXNoRepackWithMatch" skipWhileYMinusNatXNoRepackWithMatch
  , testProperty "backtrackWithDrift" backtrackWithDrift
  ]
