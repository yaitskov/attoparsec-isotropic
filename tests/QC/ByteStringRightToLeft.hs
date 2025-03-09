{-# LANGUAGE BangPatterns, CPP, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, TemplateHaskell #-}
module QC.ByteStringRightToLeft (tests) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>))
#endif
import Data.Word (Word8)
import Data.Int (Int64)
import Prelude hiding (take, takeWhile)
import QC.Common (parseBsBack, parseLbsBack, toStrictBS, liftOp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Data.Attoparsec.ByteString ((>*<))
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P8
import qualified Data.Attoparsec.ByteString.FastSet as S
import qualified Data.Attoparsec.ByteString.Lazy as PL

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

word8 :: Word8 -> B.ByteString -> Property
word8 w s = parseBsBack (P.word8 w) (B.snoc s w) === Just w

satisfy :: Word8 -> B.ByteString -> Property
satisfy w s = parseBsBack (P.satisfy (<=w)) (B.snoc s w) === Just w

stringL :: L.ByteString -> L.ByteString -> Property
stringL s t = parseLbsBack (P.string s') (t `L.append` s) === Just s'
  where s' = toStrictBS s

word8L :: Word8 -> L.ByteString -> Property
word8L w s = parseLbsBack (P.word8 w) (L.snoc s w) === Just w

satisfyL :: Word8 -> L.ByteString -> Property
satisfyL w s = parseLbsBack (P.satisfy (<=w)) (L.snoc s w) === Just w

skipL :: Word8 -> L.ByteString -> Property
skipL w s =
  case (parseLbsBack (P.skip (<w)) s, L.unsnoc s) of
    (Nothing, mcs) -> maybe (property True) (expectFailure . it) mcs
    (Just _,  mcs) -> maybe (property False) it mcs
  where it cs = liftOp "<" (<) (snd cs) w

anyWord8L :: L.ByteString -> Property
anyWord8L s
    | L.null s  = p === Nothing
    | otherwise = p === Just (L.last s)
  where p = parseLbsBack P.anyWord8 s

notWord8L :: Word8 -> NonEmptyList Word8 -> Property
notWord8L w (NonEmpty s) = parseLbsBack (P.notWord8 w) bs === if v == w
                                                        then Nothing
                                                        else Just v
    where v = L.last bs
          bs = L.pack s

peekWord8L :: L.ByteString -> Property
peekWord8L s
    | L.null s  = p === Just (s, Nothing)
    | otherwise = p === Just (s, Just (L.last s))
  where p = parseLbsBack (P.takeLazyByteString >*< P.peekWord8) s

skipWhileL :: Word8 -> L.ByteString -> Property
skipWhileL w s =
    let t = L.dropWhile (<=w) s
    in case PL.parse (P.skipWhile (<=w)) s of
         PL.Done t' () -> t === t'
         _             -> property False

takeCountL :: Positive Int -> L.ByteString -> Property
takeCountL (Positive k) s =
    case parseLbsBack (P.take k) s of
      Nothing -> liftOp ">" (>) (fromIntegral k) (L.length s)
      Just _s -> liftOp "<=" (<=) (fromIntegral k) (L.length s)

takeWhileL :: Word8 -> L.ByteString -> Property
takeWhileL w s =
    let (h,t) = L.span (==w) s
    in case PL.parse (P.takeWhile (==w)) s of
         PL.Done t' h' -> t === t' .&&. toStrictBS h === h'
         _             -> property False

takeL :: Int -> L.ByteString -> Property
takeL n s = maybe (property $ L.length s < fromIntegral n)
           (=== B.takeEnd n (toStrictBS s)) $
           parseLbsBack (P.take n) s

foo :: L.ByteString -> Maybe B.ByteString
foo = parseLbsBack P.takeByteString

takeByteStringL :: L.ByteString -> Property
takeByteStringL s = maybe (property False) (=== toStrictBS s) . foo $ s

takeLazyByteStringL :: L.ByteString -> Property
takeLazyByteStringL s = maybe (property False) (=== s) .
                       parseLbsBack P.takeLazyByteString $ s

takeWhile1L :: Word8 -> L.ByteString -> Property
takeWhile1L w s =
    let s'    = L.cons w s
        (h,t) = L.span (<=w) s'
    in case PL.parse (P.takeWhile1 (<=w)) s' of
         PL.Done t' h' -> t === t' .&&. toStrictBS h === h'
         _             -> property False

takeWhileIncludingL :: Word8 -> L.ByteString -> Property
takeWhileIncludingL w s =
    let s'    = L.cons w $ L.snoc s (w+1)
        (h_,t_) = L.span (<=w) s'
        (h,t) =
          case L.uncons t_ of
            Nothing -> (h_, t_)
            Just (n, nt) -> (h_ `L.snoc` n, nt)
    in w < 255 ==> case PL.parse (P.takeWhileIncluding (<=w)) s' of
         PL.Done t' h' -> t === t' .&&. toStrictBS h === h'
         _             -> property False

takeTillL :: Word8 -> L.ByteString -> Property
takeTillL w s =
    let (h,t) = L.break (==w) s
    in case PL.parse (P.takeTill (==w)) s of
         PL.Done t' h' -> t === t' .&&. toStrictBS h === h'
         _             -> property False

takeWhile1_emptyL :: Property
takeWhile1_emptyL = parseLbsBack (P.takeWhile1 undefined) L.empty === Nothing

getChunkL :: L.ByteString -> Property
getChunkL s =
  maybe (property False) (=== L.toChunks s) $
    parseLbsBack getChunks s
  where getChunks = go []
        go res = do
          mchunk <- P.getChunk
          case mchunk of
            Nothing -> return res
            Just chunk -> do
              _ <- P.take (B.length chunk)
              go (chunk:res)

endOfInputL :: L.ByteString -> Property
endOfInputL s = parseLbsBack P.endOfInput s === if L.null s
                                          then Just ()
                                          else Nothing

endOfInput :: B.ByteString -> Property
endOfInput s = P.parseBackOnly P.endOfInput s === if B.null s
                                          then Right ()
                                          else Left "endOfInput"

endOfLineL :: L.ByteString -> Property
endOfLineL s =
  case (parseLbsBack P8.endOfLine s, L8.uncons s) of
    (Nothing, mcs) -> maybe (property True) (expectFailure . eol) mcs
    (Just _,  mcs) -> maybe (property False) eol mcs
  where eol (c,s') = c === '\n' .||.
                     (c, fst <$> L8.uncons s') === ('\r', Just '\n')

scanL :: L.ByteString -> Positive Int64 -> Property
scanL s (Positive k) =
  parseLbsBack p s === Just (toStrictBS $ L.takeEnd k s)
  where p = P.scan k $ \ n _ ->
            if n > 0 then let !n' = n - 1 in Just n' else Nothing

membersL :: [Word8] -> Property
membersL s = property $ all (`S.memberWord8` set) s
    where set = S.fromList s

nonmembersL :: [Word8] -> [Word8] -> Property
nonmembersL s s' = property . not . any (`S.memberWord8` set) $ filter (not . (`elem` s)) s'
    where set = S.fromList s

tests :: [TestTree]
tests =
  [ testGroup "strict"
    [ testProperty "word8" word8
    , testProperty "satisfy" satisfy
    , testProperty "endOfInput" endOfInput
    ]
  , testGroup "lazy"
    [ testProperty "word8" word8L
    , testProperty "anyWord8" anyWord8L
    , testProperty "notWord8" notWord8L
    , testProperty "peekWord8" peekWord8L
    , testProperty "endOfInput" endOfInputL
    , testProperty "endOfLine" endOfLineL
    , testProperty "satisfy" satisfyL
    , testProperty "string" stringL
    , testProperty "skip" skipL
    , testProperty "scan" scanL
    , testProperty "skipWhile" skipWhileL
    , testProperty "string" stringL
    , testProperty "just-take" takeL
    , testProperty "takeByteString" takeByteStringL
    , testProperty "takeCount" takeCountL
    , testProperty "takeLazyByteString" takeLazyByteStringL
    , testProperty "takeTill" takeTillL
    , testProperty "takeWhile" takeWhileL
    , testProperty "takeWhile1" takeWhile1L
    , testProperty "takeWhile1_empty" takeWhile1_emptyL
    , testProperty "takeWhileIncluding" takeWhileIncludingL
    , testProperty "getChunk" getChunkL
    , testProperty "word8" word8L
    , testProperty "members" membersL
    , testProperty "nonmembers" nonmembersL
    ]
  ]
