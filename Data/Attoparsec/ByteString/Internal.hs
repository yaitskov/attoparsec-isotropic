{-# LANGUAGE BangPatterns, CPP, GADTs, OverloadedStrings, RankNTypes,
    RecordWildCards, TypeApplications, ScopedTypeVariables, KindSignatures,
    DataKinds, FlexibleContexts, PolyKinds, ConstraintKinds, FlexibleInstances #-}
-- |
-- Module      :  Data.Attoparsec.ByteString.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.ByteString.Internal
    (
    -- * Parser types
      Parser
    , BackParser
    , DirParser
    , Directed
    , Dir (..)
    , Buf.DefaultDrift (..)
    , Result
    , BsParserCon

    -- * Running parsers
    , parse
    , parseBack
    , parseOnly
    , parseBackOnly
    , dirParse

    -- * Combinators
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , satisfy
    , satisfyWith
    , anyWord8
    , skip
    , word8
    , notWord8

    -- ** Lookahead
    , peekWord8
    , peekWord8'

    -- ** Byte classes
    , inClass
    , notInClass

    -- * Parsing more complicated structures
    , storable

    -- * Efficient string handling
    , skipWhile
    , string
    , stringCI
    , take
    , scan
    , runScanner
    , takeWhile
    , takeWhile1
    , takeWhileIncluding
    , takeTill
    , getChunk

    -- ** Consume all remaining input
    , takeByteString
    , takeLazyByteString

    -- * Monoidal combinator
    , DirectedTuple(..)
    , (>*)
    , (*<)
    -- * Utilities
    , endOfLine
    , endOfInput
    , match
    , atEnd
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (when)
import Data.Attoparsec.ByteString.Buffer (DirBuffer, buffer', DefaultDrift (..))
import Data.Attoparsec.ByteString.FastSet (charClass, memberWord8)
import Data.Attoparsec.Combinator ((<?>))
import Data.Attoparsec.Internal
import Data.Attoparsec.Internal.Compat
import Data.Attoparsec.Internal.Fhthagn (inlinePerformIO)
import Data.Attoparsec.Internal.Types hiding (DirParser, Parser, Failure, Success, DirFailure, DirSuccess)
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Tagged (Tagged(..), untag)
import Data.Tuple (swap)
import Data.Word (Word8)
import Debug.TraceEmbrace
import qualified Foreign.ForeignPtr as FP
import Foreign.Ptr (Ptr, castPtr)
import qualified Foreign.Ptr as FP
import Foreign.Storable (Storable(peek, sizeOf))
import Prelude hiding (getChar, succ, take, takeWhile, span, drop, length, reverse)
import qualified Prelude as P
import qualified Data.Attoparsec.ByteString.Buffer as Buf
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B
import Data.Proxy

type Parser  = T.Parser ByteString
type DirParser d = T.DirParser d ByteString
type BackParser = DirParser Backward
type Result = IResult ByteString
type DirFailure d r = T.DirFailure d ByteString (DirBuffer d) r
type DirSuccess d a r = T.DirSuccess d ByteString (DirBuffer d) a r


class IntLength a where
  length :: a -> Int

instance IntLength ByteString where
  length = B.length
  {-# INLINE length #-}
instance IntLength a => IntLength (Tagged t a) where
  length  = length . untag
  {-# INLINE length #-}

spanTag :: forall k (t :: k) a b c. (a -> (b, c)) -> Tagged t a -> (Tagged t b, Tagged t c)
spanTag f (Tagged a) =
  case f a of
    (b, c) -> (Tagged b, Tagged c)

withForeignPtr :: Tagged t (FP.ForeignPtr a) -> (Tagged t (Ptr a) -> IO b) -> IO b
withForeignPtr fp f = FP.withForeignPtr (untag fp) (f . Tagged)

class DirectedPlus d => Directed (d :: Dir) where
  diffLen ::
    DirPos d -> -- ^ new position
    DirPos d -> -- ^ origin position
    DirPos d

  startPos :: ByteString -> DirPos d
  isNotAll :: DirPos d -> Int -> Bool
  substring :: DirPos d -> DirPos d -> DirBuffer d -> Tagged d ByteString
  lengthAtLeast :: DirPos d -> Int -> DirBuffer d -> Bool
  span :: (Word8 -> Bool) -> Tagged d ByteString -> (Tagged d ByteString, Tagged d ByteString)
  uncons :: Tagged d ByteString -> Maybe (Word8, ByteString)
  snoc :: Tagged d ByteString -> Word8 -> ByteString
  takeWhileD :: (Word8 -> Bool) -> Tagged d ByteString -> Tagged d ByteString
  peekRest :: DirPos d -> DirBuffer d -> Tagged d ByteString
  isPrefixOf :: Tagged d ByteString -> Tagged d ByteString -> Bool
  drop :: Int -> Tagged d ByteString -> Tagged d ByteString
  unsafeTake :: Int -> Tagged d ByteString -> Tagged d ByteString
  unsafeDrop :: Int -> Tagged d ByteString -> Tagged d ByteString
  reverse :: [Tagged d ByteString] -> [Tagged d ByteString]
  scanner :: s -> Tagged d ByteString -> (s -> Word8 -> Maybe s) -> IO (T s)

instance Directed Forward where
  diffLen np op = np - op
  {-# INLINE diffLen #-}
  startPos _ = Pos 0
  {-# INLINE startPos #-}
  isNotAll (Pos p) n = p < n
  {-# INLINE isNotAll #-}
  substring (Pos pos) (Pos n) = Tagged . Buf.substring pos n
  {-# INLINE substring #-}
  lengthAtLeast (Pos pos) n bs = Buf.length bs >= pos + n
  {-# INLINE lengthAtLeast #-}
  span f = spanTag (B8.span f)
  {-# INLINE span #-}
  uncons (Tagged bs) = B8.uncons bs
  {-# INLINE uncons #-}
  snoc (Tagged bs) = B8.snoc bs
  {-# INLINE snoc #-}
  takeWhileD p bs = B8.takeWhile p <$> bs
  {-# INLINE takeWhileD #-}
  peekRest (Pos d) =  Tagged . Buf.unsafeDrop d
  {-# INLINE peekRest #-}
  isPrefixOf (Tagged pre) (Tagged bs) = B.isPrefixOf pre bs
  {-# INLINE isPrefixOf #-}
  drop n bs = B.drop n <$> bs
  {-# INLINE drop #-}
  unsafeTake n bs = B.unsafeTake n <$> bs
  {-# INLINE unsafeTake #-}
  unsafeDrop n bs = B.unsafeDrop n <$> bs
  {-# INLINE unsafeDrop #-}
  reverse = P.reverse
  {-# INLINE reverse #-}
  scanner s1 bs p =
    withPsTag bs $ \fp off len ->
      withForeignPtr fp $ \ptr0 -> do
        let start = untag ptr0 `FP.plusPtr` off
            end   = untag ptr0 `FP.plusPtr` (off + len)
            inner ptr !s
              | ptr < end = do
                w <- peek ptr
                case p s w of
                  Just s' -> inner (ptr `FP.plusPtr` 1) s'
                  _       -> done (ptr `FP.minusPtr` start) s
              | otherwise = done (ptr `FP.minusPtr` start) s
            done !i !s = return (T i s)
        inner start s1
  {-# INLINE scanner #-}

instance Directed Backward where
  diffLen np op = op - np
  {-# INLINE diffLen #-}
  startPos = Pos . (\x -> x - 1) . B.length
  {-# INLINE startPos #-}
  isNotAll a n = a >= 0 && n > 0
  {-# INLINE isNotAll #-}
  substring (Pos pos) (Pos n) b =
    $(tw "substring/pos n b")
      (Tagged (Buf.substring (pos + 1 - n) n b))
  {-# INLINE substring #-}
  lengthAtLeast (Pos pos) n _bs = pos + 1 - n >= 0
  {-# INLINE lengthAtLeast #-}
  span p = spanTag (B8.spanEnd p)
  {-# INLINE span #-}
  uncons (Tagged bs) = swap <$> B8.unsnoc bs
  {-# INLINE uncons #-}
  snoc (Tagged bs) b = B8.cons b bs
  {-# INLINE snoc #-}
  takeWhileD p bs = B8.takeWhileEnd p <$> bs
  {-# INLINE takeWhileD #-}
  peekRest (Pos d) b =
    $(tw "/d b") (Tagged (Buf.substring 0 (d + 1) b))
  {-# INLINE peekRest #-}
  isPrefixOf (Tagged pre) (Tagged bs) = B.isSuffixOf pre bs
  {-# INLINE isPrefixOf #-}
  drop n bs = B.dropEnd n <$> bs
  {-# INLINE drop #-}
  -- unsafeTakeEnd
  unsafeTake n bs = B.unsafeDrop (B.length (untag bs) - n) <$> bs
  {-# INLINE unsafeTake #-}
  -- unsafeDropEnd
  unsafeDrop n bs = B.unsafeTake (B.length (untag bs) - n) <$> bs
  {-# INLINE unsafeDrop #-}
  reverse = id
  {-# INLINE reverse #-}
  scanner s1 bs p =
    withPsTag bs $ \fp off len ->
      withForeignPtr fp $ \(Tagged ptr0) ->
        if ptr0 == FP.nullPtr
        then pure $ T 0 s1
        else do
          let start = ptr0 `FP.plusPtr` (off + len - 1)
              end   = ptr0 `FP.plusPtr` off
              inner ptr !s
                | ptr >= end = do
                  w <- peek ptr
                  case p s w of
                    Just s' -> inner (ptr `FP.plusPtr` (-1)) s'
                    _       -> done (start `FP.minusPtr` ptr) s
                | otherwise = done (start `FP.minusPtr` ptr) s
              done !i !s = return (T i s)
          inner start s1
  {-# INLINE scanner #-}

class Directed d => DirectedTuple d where
  -- | Run first then last parser in Forward mode and vice-versa in Backward one.
  -- Monoidal operator
  (>*<) :: DirParser d a -> DirParser d b -> DirParser d (a, b)

infixl 4 >*<

instance DirectedTuple Forward where
  a >*< b = (,) <$> a <*> b
  {-# INLINE (>*<) #-}

instance DirectedTuple Backward where
  a >*< b = b >>= \br -> a >>= \ar -> pure (ar, br)
  {-# INLINE (>*<) #-}

(>*) :: DirectedTuple d => DirParser d a -> DirParser d b -> DirParser d b
a >* b = snd <$> (a >*< b)
infixl 4 >*

(*<) :: DirectedTuple d => DirParser d a -> DirParser d b -> DirParser d a
a *< b = fst <$> (a >*< b)
infixl 4 *<

type BsParserCon d =
  ( DirChunk d ByteString
  , Directed d
  , DirBuffer d ~ DirState d ByteString
  , Buf.HasDrift d
  , Alternative (DirParser d)
  )

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: BsParserCon d => (Word8 -> Bool) -> DirParser d Word8
satisfy p = do
  h <- peekWord8'
  if $(tw "pokedWord/h") $ p h
    then advance ($(tw "advance by/") 1)  >> return h
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @skip p@ succeeds for any byte for which the predicate
-- @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit w = w >= 48 && w <= 57
skip :: BsParserCon d => (Word8 -> Bool) -> DirParser d ()
skip p = do
  h <- peekWord8'
  if $(tw "pokedWord/h") $ p h
    then advance $ $(tw "advance by/") 1
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
satisfyWith :: (Show a, BsParserCon d) => (Word8 -> a) -> (a -> Bool) -> DirParser d a
satisfyWith f p = do
  h <- peekWord8'
  let c = f h
  if $(tw "p c ret/h c") $ p c
    then advance 1 >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

storable :: (BsParserCon d, Storable a) => DirParser d a
storable = hack undefined
 where
  hack :: (BsParserCon d, Storable b) => b -> DirParser d b
  hack dummy = do
    (fp,o,_) <- B.toForeignPtr `fmap` take (sizeOf dummy)
    return . inlinePerformIO . FP.withForeignPtr fp $ \p ->
        peek (castPtr $ p `FP.plusPtr` o)

-- | Consume exactly @n@ bytes of input.
take :: BsParserCon d => Int -> DirParser d ByteString
take n0 = do
  let n = max n0 0
  s <- ensure n
  advance ($(tw "/n0 n") n)  >> return s
{-# INLINE take #-}

-- | @string s@ parses a sequence of bytes that identically match
-- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- input if it fails (even if a partial match).
--
-- /Note/: The behaviour of this parser is different to that of the
-- similarly-named parser in Parsec, as this one is all-or-nothing.
-- To illustrate the difference, the following parser will fail under
-- Parsec given an input of @\"for\"@:
--
-- >string "foo" <|> string "for"
--
-- The reason for its failure is that the first branch is a
-- partial match, and will consume the letters @\'f\'@ and @\'o\'@
-- before failing.  In attoparsec, the above parser will /succeed/ on
-- that input, because the failed first branch will consume nothing.
string :: BsParserCon d => ByteString -> DirParser d ByteString
string s = string_ (stringSuspended id) id (Tagged s)
{-# INLINE string #-}

-- ASCII-specific but fast, oh yes.
toLower :: Word8 -> Word8
toLower w | w >= 65 && w <= 90 = w + 32
          | otherwise          = w

-- | Satisfy a literal string, ignoring case.
stringCI :: BsParserCon d => ByteString -> DirParser d ByteString
stringCI s = string_ (stringSuspended lower) lower (Tagged s)
  where lower = fmap (B8.map toLower)
{-# INLINE stringCI #-}

string_ :: forall (d :: Dir). BsParserCon d
        => (forall r. Tagged d ByteString -> Tagged d ByteString -> DirBuffer d -> DirPos d -> More
            -> DirFailure d r -> DirSuccess d ByteString r -> Result r)
        -> (Tagged d ByteString -> Tagged d ByteString)
        -> Tagged d ByteString
        -> DirParser d ByteString
string_ suspended f s0 = T.Parser $ \t pos more lose succ ->
  let n = B.length $ untag s
      s = f s0
  in if $(tw "/pos n s t") $ lengthAtLeast pos n t
     then let t' = substring pos (Pos n) t
          in if s == f t'
             then succ t (pos + (there (Pos n))) more (untag t')
             else lose t pos more [] "string"
     else let t' = peekRest pos t
          in if f t' `isPrefixOf` s
             then suspended s (drop (length t') s) t pos more lose succ
             else lose t pos more [] "string"
{-# INLINE string_ #-}

stringSuspended :: forall d r. BsParserCon d
                => (Tagged d ByteString -> Tagged d ByteString)
                -> Tagged d ByteString
                -> Tagged d ByteString
                -> DirBuffer d -> DirPos d -> More
                -> DirFailure d r
                -> DirSuccess d ByteString r
                -> Result r
stringSuspended f s0 s t pos more lose succ =
    runParser (demandInput_ >>= go . Tagged) t pos more lose succ
  where go s'0   = T.Parser $ \t' pos' more' lose' succ' ->
          let m  = length s
              s' = f s'0
              n  = length s'
          in if $(tw "/t t' pos pos' s' n m ") $ n >= m
             then if unsafeTake m s' == s
                  then let o = Pos (length s0)
                       in succ' t' (pos' + there o) more'
                          (untag $ substring pos' o t')
                  else lose' t' pos' more' [] "string"
             else if s' == unsafeTake n s
                  then stringSuspended f s0 (unsafeDrop n s)
                       t' pos' more' lose' succ'
                  else lose' t' pos' more' [] "string"

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: BsParserCon d => (Word8 -> Bool) -> DirParser d ()
skipWhile p = go
 where
  go = do
    t <- $(tw "tookWhileD/") . takeWhileD p <$> get
    continue <- $(tw "continue/") <$> inputSpansChunks (length $ untag t)
    when continue go
{-# INLINE skipWhile #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
takeTill :: BsParserCon d => (Word8 -> Bool) -> DirParser d ByteString
takeTill p = takeWhile (not . p)
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
takeWhile :: BsParserCon d => (Word8 -> Bool) -> DirParser d ByteString
takeWhile p = do
    s <- takeWhileD p <$> get
    continue <- inputSpansChunks (B.length $ untag s)
    if continue
      then takeWhileAcc p [s]
      else return $ untag s
{-# INLINE takeWhile #-}

takeWhileAcc :: forall d. BsParserCon d
             => (Word8 -> Bool)
             -> [Tagged d ByteString]
             -> DirParser d ByteString
takeWhileAcc p = go
 where
  go acc = do
    s <- takeWhileD p <$> get
    continue <- inputSpansChunks (B.length $ untag s)
    if $(tw "continue/s acc") continue
      then go (s:acc)
      else pure . untag $ concatReverse (s:acc)
{-# INLINE takeWhileAcc #-}

-- | Consume input until immediately after the predicate returns 'True', and return
-- the consumed input.
--
-- This parser will consume at least one 'Word8' or fail.
takeWhileIncluding :: BsParserCon d => (Word8 -> Bool) -> DirParser d B.ByteString
takeWhileIncluding p = do
  (s', t) <- span p <$> get
  case uncons t of
    -- Since we reached a break point and managed to get the next byte,
    -- input can not have been exhausted thus we succed and advance unconditionally.
    Just (h, _) -> do
      let s = s' `snoc` h
      advance $ $(tw "by len/s") (B8.length s)
      return s
    -- The above isn't true so either we ran out of input or we need to process the next chunk.
    Nothing -> do
      continue <- inputSpansChunks (length s')
      if continue
        then takeWhileIncAcc p [s']
        -- Our spec says that if we run out of input we fail.
        else fail "takeWhileIncluding reached end of input"
{-# INLINE takeWhileIncluding #-}

takeWhileIncAcc :: BsParserCon d
                => (Word8 -> Bool)
                -> [Tagged d B.ByteString]
                -> DirParser d B.ByteString
takeWhileIncAcc p = go
 where
   go acc = do
     (s', t) <- span p <$> get
     case uncons t of
       Just (h, _) -> do
         let s = s' `snoc` h
         advance $ $(tw "by len /s") (B8.length s)
         return $ untag (concatReverse $ (Tagged s) : acc)
       Nothing -> do
         continue <- inputSpansChunks (length s')
         if continue
           then go (s':acc)
           else fail "takeWhileIncAcc reached end of input"
{-# INLINE takeWhileIncAcc #-}



takeRest :: BsParserCon d => DirParser d [Tagged d ByteString]
takeRest = $(tw "/") <$> go []
  where
  go acc = do
    input <- wantInput
    if input
      then do
        s <- get
        advance $ $(tw "by len/s") (length s)
        go (s:acc)
      else return (reverse acc)

-- | Consume all remaining input and return it as a single string.
takeByteString :: BsParserCon d => DirParser d ByteString
takeByteString = (untag . mconcat) `fmap` takeRest

-- | Consume all remaining input and return it as a single string.
takeLazyByteString :: BsParserCon d => DirParser d L.ByteString
takeLazyByteString = (L.fromChunks . fmap untag) `fmap` takeRest

-- | Return the rest of the current chunk without consuming anything.
--
-- If the current chunk is empty, then ask for more input.
-- If there is no more input, then return 'Nothing'
getChunk :: BsParserCon d => DirParser d (Maybe ByteString)
getChunk = do
  input <- wantInput
  if input
    then Just . untag <$> get
    else return Nothing

data T s = T {-# UNPACK #-} !Int s

scan_ ::
  forall d s r. (Show s, BsParserCon d) =>
  (s -> [Tagged d ByteString] -> DirParser d r) ->
  s ->
  (s -> Word8 -> Maybe s) ->
  DirParser d r
scan_ f s0 p = go ($(tr "/s0") []) s0
 where
  go acc s1 = do
    bs <- $(tw "got bs/") <$> get
    let T i s' = inlinePerformIO $ scanner s1 bs p
        !h = unsafeTake i $ $(tr "/i bs") bs
    continue <- inputSpansChunks $ $(tr "/i h") i
    if $(tr "/i s' h continue") continue
      then go (h:acc) s'
      else f s' (h:acc)
{-# INLINE scan_ #-}

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
scan :: (Show s, BsParserCon d)
     => s -> (s -> Word8 -> Maybe s) -> DirParser d ByteString
scan = scan_ $ \_ chunks -> return $! untag (concatReverse chunks)
{-# INLINE scan #-}

-- | Like 'scan', but generalized to return the final state of the
-- scanner.
runScanner :: (Show s, BsParserCon d)
           => s -> (s -> Word8 -> Maybe s) -> DirParser d (ByteString, s)
runScanner = scan_ $ \s xs -> let !sx = concatReverse xs in return (untag sx, s)
{-# INLINE runScanner #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
takeWhile1 :: BsParserCon d => (Word8 -> Bool) -> DirParser d ByteString
takeWhile1 p = do
  (`when` demandInput) =<< endOfChunk
  s <- takeWhileD p <$> get
  let len = B.length $ untag s
  if $(tw "/s len") $ len == 0
    then fail "takeWhile1"
    else do
      advance $ $(tw "by/len") len
      eoc <- endOfChunk
      if $(tw "eoc/") eoc
        then takeWhileAcc p [s]
        else return $ untag s
{-# INLINE takeWhile1 #-}

-- | Match any byte in a set.
--
-- >vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- >halfAlphabet = inClass "a-nA-N"
--
-- To add a literal @\'-\'@ to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Word8 -> Bool
inClass s = (`memberWord8` mySet)
    where mySet = charClass s
          {-# NOINLINE mySet #-}
{-# INLINE inClass #-}

-- | Match any byte not in a set.
notInClass :: String -> Word8 -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Match any byte.
anyWord8 :: BsParserCon d => DirParser d Word8
anyWord8 = satisfy $ const True
{-# INLINE anyWord8 #-}

-- | Match a specific byte.
word8 :: BsParserCon d => Word8 -> DirParser d Word8
word8 c = satisfy (== c) <?> show c
{-# INLINE word8 #-}

-- | Match any byte except the given one.
notWord8 :: BsParserCon d => Word8 -> DirParser d Word8
notWord8 c = satisfy (/= c) <?> "not " ++ show c
{-# INLINE notWord8 #-}

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
peekWord8 :: BsParserCon d => DirParser d (Maybe Word8)
peekWord8 = T.Parser $ \t pos@(Pos pos_) more _lose succ ->
  case () of
    _| isNotAll pos (Buf.length t) ->
       let !w = Buf.unsafeIndex t pos_
       in succ t pos more (Just w)
     | more == Complete ->
       succ t pos more Nothing
     | otherwise ->
       let succ' t' pos' more' = let !w = Buf.unsafeIndex t' pos_
                                 in succ t' pos' more' (Just w)
           lose' t' pos' more' = succ t' pos' more' Nothing
       in prompt t pos more lose' succ'
{-# INLINE peekWord8 #-}

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
peekWord8' :: BsParserCon d => DirParser d Word8
peekWord8' = T.Parser $ \t pos more lose succ ->
    if lengthAtLeast pos 1 t
    then succ t pos more (Buf.unsafeIndex t (fromPos pos))
    else let succ' t' pos' more' bs' = succ t' pos' more' $! B.unsafeHead bs'
         in ensureSuspended 1 t pos more lose succ'
{-# INLINE peekWord8' #-}

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: BsParserCon d => DirParser d ()
endOfLine = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

-- | Terminal failure continuation.
failK :: BsParserCon d => DirFailure d a
failK t pos _more stack msg = Fail (untag $ peekRest pos t) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: BsParserCon d => DirSuccess d a a
successK t pos _more a = Done (untag $ peekRest pos t) a
{-# INLINE successK #-}

-- | Run a parser.
dirParse ::
  forall d a. (Buf.DefaultDrift d, BsParserCon d) =>
  DirParser d a -> ByteString -> Result a
dirParse m s = T.runParser m b sp Incomplete failK successK
  where
    sp = $(tw "dirParse startPos/s") $ startPos s
    b = buffer' (initDrift (Proxy @d)) s
{-# INLINE dirParse #-}

parse :: Parser a -> ByteString -> Result a
parse = dirParse
{-# INLINE parse #-}

parseBack :: BackParser a -> ByteString -> Result a
parseBack = dirParse
{-# INLINE parseBack #-}

dirParseOnly ::
  forall d a. (Buf.DefaultDrift d, BsParserCon d) =>
  DirParser d a -> ByteString -> Either String a
dirParseOnly m s =
  case T.runParser m b (startPos s) Complete failK successK of
                  Fail _ [] err   -> Left err
                  Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
                  Done _ a        -> Right a
                  _               -> error "parseOnly: impossible error!"
  where
    b = buffer' (initDrift (Proxy @d)) s
{-# INLINE dirParseOnly #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
--
-- This function does not force a parser to consume all of its input.
-- Instead, any residual input will be discarded.  To force a parser
-- to consume all of its input, use something like this:
--
-- @
--'parseOnly' (myParser 'Control.Applicative.<*' 'endOfInput')
-- @
parseOnly :: Parser a -> ByteString -> Either String a
parseOnly = dirParseOnly
{-# INLINE parseOnly #-}

parseBackOnly :: BackParser a -> ByteString -> Either String a
parseBackOnly = dirParseOnly
{-# INLINE parseBackOnly #-}

get :: BsParserCon d => DirParser d (Tagged d ByteString)
get = T.Parser $ \t pos more _lose succ ->
  succ t pos more (peekRest ($(tw "from position/t") pos) t)
{-# INLINE get #-}

endOfChunk :: BsParserCon d => DirParser d Bool
endOfChunk = T.Parser $ \t pos more _lose succ ->
  succ t pos more $ $(tw "for/pos t") (not (isNotAll pos (Buf.length t)))
{-# INLINE endOfChunk #-}

inputSpansChunks :: BsParserCon d => Int -> DirParser d Bool
inputSpansChunks i = T.Parser $ \t pos_ more _lose succ ->
  let pos = pos_ + there (Pos i)
  in if $(tw "/more pos pos_ i t") (isNotAll pos (Buf.length t) || more == Complete)
     then succ t pos more False
     else let lose' t' pos' more' = succ t' pos' more' False
              succ' t' pos' more' = succ t' pos' more' True
          in prompt t pos more lose' succ'
{-# INLINE inputSpansChunks #-}

advance :: BsParserCon d => Int -> DirParser d ()
advance n = T.Parser $ \t pos more _lose succ ->
  succ t (pos + $(tw "ret by/more pos n t") (there (Pos n))) more ()
{-# INLINE advance #-}

ensureSuspended :: BsParserCon d
                => Int -> DirBuffer d -> DirPos d -> More
                -> DirFailure d r
                -> DirSuccess d ByteString r
                -> Result r
ensureSuspended n t pos more lose succ =
    runParser (demandInput >> go) t pos more lose succ
  where go = T.Parser $ \t' pos' more' lose' succ' ->
          if $(tw "/t' pos' t pos n") $ lengthAtLeast pos' n t'
          then succ' t' pos' more' (untag $ substring pos' (Pos n) t')
          else runParser (demandInput >> go) t' pos' more' lose' succ'

-- | If at least @n@ elements of input are available, return the
-- current input, otherwise fail.
ensure :: BsParserCon d => Int -> DirParser d ByteString
ensure n = T.Parser $ \t pos more lose succ ->
    if $(tw "/n pos") $ lengthAtLeast pos n t
    then succ t pos more (untag $ substring pos (Pos n) t)
    -- The uncommon case is kept out-of-line to reduce code size:
    else ensureSuspended n t pos more lose succ
{-# INLINE ensure #-}

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
match :: BsParserCon d => DirParser d a -> DirParser d (ByteString, a)
match p = T.Parser $ \t pos more lose succ ->
  let drift = Buf.getDrift t
      succ' t' pos' more' a =
        let drift' = Buf.getDrift t'
            dd = Pos $ drift' - drift
            posD = diffLen pos' pos
         in
        --   pos' = -1 pos = 0 => 0 - (-1) => n = 1
        --       (Tagged (Buf.substring (pos + 1 - n) n b))
          succ t' pos' more'
          ( untag $ substring (pos + dd) (dd + posD) $
            $(tr "/drift drift' pos pos' t t' dd posD") t'
          , a
          )
  in runParser p t pos more lose succ'
