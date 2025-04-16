{-# LANGUAGE CPP, BangPatterns, GeneralizedNewtypeDeriving, OverloadedStrings,
    Rank2Types, RecordWildCards, TypeFamilies, DataKinds, TypeApplications,
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
-- |
-- Module      :  Data.Attoparsec.Internal.Types
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators, loosely based on the Parsec
-- library.

module Data.Attoparsec.Internal.Types
    (
      Parser
    , DirParser(..)
    , State
    , DirState
    , Failure
    , Success
    , DirFailure
    , DirSuccess
    , Pos
    , IResult(..)
    , More(..)
    , (<>)
    , Chunk(..)
    , DirChunk(..)
    , Dir(..)
    , DirPos(..)
    , DirectedPlus(..)
    ) where

import Control.Applicative as App (Applicative(..))
import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail (MonadFail(..))
import Data.Monoid as Mon (Monoid(..))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Tagged (Tagged(..), untag)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Unsafe (Iter(..))
import Debug.TraceEmbrace
import Prelude hiding (succ)
import Data.Attoparsec.ByteString.Buffer (Dir (..))
import qualified Data.Attoparsec.ByteString.Buffer as B
import qualified Data.Attoparsec.Text.Buffer as T


newtype DirPos (d :: Dir) = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

class DirectedPlus (d :: Dir) where
  there :: DirPos d -> DirPos d

instance DirectedPlus Forward where
  there = id

instance DirectedPlus Backward where
  there = negate

type Pos = DirPos Forward

-- | The result of a parse.  This is parameterised over the type @i@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult i r =
    Fail i [String] String
    -- ^ The parse failed.  The @i@ parameter is the input that had
    -- not yet been consumed when the failure occurred.  The
    -- @[@'String'@]@ is a list of contexts in which the error
    -- occurred.  The 'String' is the message describing the error, if
    -- any.
  | Partial (i -> IResult i r)
    -- ^ Supply this continuation with more input so that the parser
    -- can resume.  To indicate that no more input is available, pass
    -- an empty string to the continuation.
    --
    -- __Note__: if you get a 'Partial' result, do not call its
    -- continuation more than once.
  | Done i r
    -- ^ The parse succeeded.  The @i@ parameter is the input that had
    -- not yet been consumed (if any) when the parse succeeded.

instance (Show i, Show r) => Show (IResult i r) where
    showsPrec d ir = showParen (d > 10) $
      case ir of
        (Fail t stk msg) -> showString "Fail" . f t . f stk . f msg
        (Partial _)      -> showString "Partial _"
        (Done t r)       -> showString "Done" . f t . f r
      where f :: Show a => a -> ShowS
            f x = showChar ' ' . showsPrec 11 x

instance (NFData i, NFData r) => NFData (IResult i r) where
    rnf (Fail t stk msg) = rnf t `seq` rnf stk `seq` rnf msg
    rnf (Partial _)  = ()
    rnf (Done t r)   = rnf t `seq` rnf r
    {-# INLINE rnf #-}

instance Functor (IResult i) where
    fmap _ (Fail t stk msg) = Fail t stk msg
    fmap f (Partial k)      = Partial (fmap f . k)
    fmap f (Done t r)   = Done t (f r)

-- | The core parser type.  This is parameterised over the type @i@
-- of string being processed.
--
-- This type is an instance of the following classes:
--
-- * 'Monad', where 'fail' throws an exception (i.e. fails) with an
--   error message.
--
-- * 'Functor' and 'Applicative', which follow the usual definitions.
--
-- * 'MonadPlus', where 'mzero' fails (with no error message) and
--   'mplus' executes the right-hand parser if the left-hand one
--   fails.  When the parser on the right executes, the input is reset
--   to the same state as the parser on the left started with. (In
--   other words, attoparsec is a backtracking parser that supports
--   arbitrary lookahead.)
--
-- * 'Alternative', which follows 'MonadPlus'.
newtype DirParser (d :: Dir) i a = Parser {
      runParser :: forall r.
                   DirState d i -> DirPos d -> More
                -> DirFailure d i (DirState d i)   r
                -> DirSuccess d i (DirState d i) a r
                -> IResult i r
    }

type Parser = DirParser Forward
type family DirState (d :: Dir) i
type instance DirState Forward ByteString = B.Buffer
type instance DirState Backward ByteString = B.DirBuffer Backward
type instance DirState Forward Text = T.Buffer

type State x = DirState Forward x

type DirFailure d i t   r =
  t -> DirPos d -> More -> [String] -> String -> IResult i r
type DirSuccess d i t a r =
  t -> DirPos d -> More -> a -> IResult i r

type Success i t a r = DirSuccess Forward i t a r
type Failure i t   r = DirFailure Forward i t   r

-- | Have we read all available input?
data More = Complete | Incomplete
            deriving (Eq, Show)

instance Semigroup More where
    c@Complete <> _ = c
    _          <> m = m

instance Mon.Monoid More where
    mappend = (<>)
    mempty  = Incomplete

instance Monad (DirParser d i) where
#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

    return = App.pure
    {-# INLINE return #-}

    m >>= k = Parser $ \t !pos more lose succ ->
        let succ' t' !pos' more' a = runParser (k a) t' pos' more' lose succ
        in runParser m t pos more lose succ'
    {-# INLINE (>>=) #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}


instance Fail.MonadFail (DirParser d i) where
    fail err = Parser $ \t pos more lose _succ -> lose t pos more [] msg
      where msg = "Failed reading: " ++ err
    {-# INLINE fail #-}

-- I see in case of failure - t' with increased drift is used with
-- old pos. What options in Backward mode?
-- 1) add drift increase to old pos?
--    Sounds spooky
-- 2) Use original t
plus :: DirParser d i a -> DirParser d i a -> DirParser d i a
plus f g = Parser $ \t pos more lose succ ->
  let lose' t' pos' more' _ctx _msg = runParser g t' ($(tr "/pos pos'") pos) more' lose succ
  in runParser f t pos more lose' succ

type BsBackParser a = DirParser Backward ByteString a

plusBack :: BsBackParser a -> BsBackParser a -> BsBackParser a
plusBack f g =
  Parser $ \t pos more lose succ' ->
             let lose' t' _pos' more' _ctx _msg =
                   let !tDrift = $(tw "t drift/") $ B.getDrift t
                       !t'Drift = $(tw "t' drift/") $ B.getDrift t'
                       !dd = $(tw "dd/") $ t'Drift - tDrift
                       pos' = $(tw "pos'/pos") $ pos + (Pos @Backward dd)
                   in runParser g t' pos' more' lose succ'
             in runParser f t pos more lose' succ'

instance MonadPlus (DirParser Forward i) where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus = plus

instance MonadPlus (DirParser Backward ByteString) where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus = plusBack

instance Functor (DirParser d i) where
    fmap f p = Parser $ \t pos more lose succ ->
      let succ' t' pos' more' a = succ t' pos' more' (f a)
      in runParser p t pos more lose succ'
    {-# INLINE fmap #-}

apP :: DirParser d i (a -> b) -> DirParser d i a -> DirParser d i b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative (DirParser d i) where
    pure v = Parser $ \t !pos more _lose succ -> succ t pos more v
    {-# INLINE pure #-}
    (<*>)  = apP
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}
    x <* y = x >>= \a -> y >> pure a
    {-# INLINE (<*) #-}

instance Semigroup (DirParser Forward i a) where
    (<>) = plus
    {-# INLINE (<>) #-}

instance Semigroup (BsBackParser a) where
    (<>) = plusBack
    {-# INLINE (<>) #-}

instance Monoid (DirParser Forward i a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Monoid (BsBackParser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Alternative (DirParser Forward i) where
    empty = fail "empty"
    {-# INLINE empty #-}

    (<|>) = plus
    {-# INLINE (<|>) #-}

    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE many #-}

    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE some #-}

instance Alternative (DirParser Backward ByteString) where
    empty = fail "empty"
    {-# INLINE empty #-}

    (<|>) = plusBack
    {-# INLINE (<|>) #-}

    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE many #-}

    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE some #-}

-- | A common interface for input chunks.
class Monoid c => Chunk c where
  type ChunkElem c
  -- | Test if the chunk is empty.
  nullChunk :: c -> Bool
  -- | Map an element to the corresponding character.
  --   The first argument is ignored.
  chunkElemToChar :: c -> ChunkElem c -> Char

instance Chunk ByteString where
  type ChunkElem ByteString = Word8
  nullChunk = BS.null
  {-# INLINE nullChunk #-}
  chunkElemToChar _ = w2c
  {-# INLINE chunkElemToChar #-}

instance Chunk Text where
  type ChunkElem Text = Char
  nullChunk = Text.null
  {-# INLINE nullChunk #-}
  chunkElemToChar _ = id
  {-# INLINE chunkElemToChar #-}


class (DirectedPlus d, Chunk c, Show (DirState d c)) => DirChunk (d :: Dir) c where
  type DirChunkElem d c
  -- | Position at the end of a buffer. The first argument is ignored.
  notAtBufferEnd :: c -> DirPos d -> DirState d c -> Bool
  -- | Return the buffer element at the given position along with its length.
  bufferElemAt :: c -> DirPos d -> DirState d c -> Maybe (DirChunkElem d c, Int)
  shiftPositionOnBufferExtend :: DirPos d -> c -> DirPos d
  -- | Append chunk to a buffer.
  pappendChunk :: DirState d c -> Tagged d c -> DirState d c
  concatReverse :: [Tagged d c] -> Tagged d c

instance DirChunk Forward ByteString where
  type DirChunkElem Forward ByteString = Word8
  notAtBufferEnd _ (Pos p) t = p < B.length t
  {-# INLINE notAtBufferEnd #-}
  bufferElemAt _ (Pos i) buf
    | i < B.length buf = Just (B.unsafeIndex buf i, 1)
    | otherwise = Nothing
  {-# INLINE bufferElemAt #-}
  shiftPositionOnBufferExtend a _ = a
  {-# INLINE shiftPositionOnBufferExtend #-}
  pappendChunk buf t = B.pappend buf (untag t)
  {-# INLINE pappendChunk #-}
  concatReverse [x] = x
  concatReverse xs = mconcat (reverse xs)
  {-# INLINE concatReverse #-}

instance DirChunk Backward ByteString where
  type DirChunkElem Backward ByteString = Word8
  notAtBufferEnd _ p t = p >= 0 && B.length t > 0
  {-# INLINE notAtBufferEnd #-}
  bufferElemAt _ (Pos i) buf
    | i >= 0 = Just (B.unsafeIndex buf i, 1)
    | otherwise = Nothing
  {-# INLINE bufferElemAt #-}
  shiftPositionOnBufferExtend a s = a + Pos (BS.length s)
  {-# INLINE shiftPositionOnBufferExtend #-}
  pappendChunk buf t = B.pepreppend buf (untag t)
  {-# INLINE pappendChunk #-}
  concatReverse [x] = x
  concatReverse xs = mconcat xs
  {-# INLINE concatReverse #-}

instance DirChunk Forward Text where
  type DirChunkElem Forward Text = Char
  notAtBufferEnd _ (Pos p) t = p < T.length t
  {-# INLINE notAtBufferEnd #-}
  bufferElemAt _ (Pos i) buf
    | i < T.length buf = let Iter c l = T.iter buf i in Just (c, l)
    | otherwise = Nothing
  {-# INLINE bufferElemAt #-}
  shiftPositionOnBufferExtend a _ = a
  {-# INLINE shiftPositionOnBufferExtend #-}
  pappendChunk buf t = T.pappend buf (untag t)
  {-# INLINE pappendChunk #-}
  concatReverse [x] = x
  concatReverse xs = mconcat (reverse xs)
  {-# INLINE concatReverse #-}
