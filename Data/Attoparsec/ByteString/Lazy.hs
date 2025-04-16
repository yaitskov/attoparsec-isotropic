{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Imports internal modules
#endif

-- |
-- Module      :  Data.Attoparsec.ByteString.Lazy
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing that can consume lazy
-- 'ByteString' strings, loosely based on the Parsec library.
--
-- This is essentially the same code as in the 'Data.Attoparsec'
-- module, only with a 'parse' function that can consume a lazy
-- 'ByteString' incrementally, and a 'Result' type that does not allow
-- more input to be fed in.  Think of this as suitable for use with a
-- lazily read file, e.g. via 'L.readFile' or 'L.hGetContents'.
--
-- /Note:/ The various parser functions and combinators such as
-- 'string' still expect /strict/ 'B.ByteString' parameters, and
-- return strict 'B.ByteString' results.  Behind the scenes, strict
-- 'B.ByteString' values are still used internally to store parser
-- input and manipulate it efficiently.

module Data.Attoparsec.ByteString.Lazy
    (
      Result(..)
    , LbsParserCon
    , module Data.Attoparsec.ByteString
    -- * Running parsers
    , parse
    , parseOnly
    , parseTest
    , dirParse
    , dirParseOnly
    , dirParseTest
    , parseBack
    , parseOnlyBack
    , parseTestBack

    -- ** Result conversion
    , maybeResult
    , eitherResult
    ) where

import Control.DeepSeq (NFData(rnf))
import Data.ByteString.Lazy.Internal (ByteString(..), chunk)
import Data.List (intercalate)
import Data.Tagged
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Internal as I
import qualified Data.Attoparsec.Internal.Types as T
import Data.Attoparsec.ByteString
    hiding (IResult(..), Result, eitherResult, maybeResult,
            parse, parseOnly, parseWith, parseTest, dirParse, parseBack)
import Debug.TraceEmbrace

-- | The result of a parse.
data Result r = Fail ByteString [String] String
              -- ^ The parse failed.  The 'ByteString' is the input
              -- that had not yet been consumed when the failure
              -- occurred.  The @[@'String'@]@ is a list of contexts
              -- in which the error occurred.  The 'String' is the
              -- message describing the error, if any.
              | Done ByteString r
              -- ^ The parse succeeded.  The 'ByteString' is the
              -- input that had not yet been consumed (if any) when
              -- the parse succeeded.

instance NFData r => NFData (Result r) where
    rnf (Fail bs ctxs msg) = rnfBS bs `seq` rnf ctxs `seq` rnf msg
    rnf (Done bs r)        = rnfBS bs `seq` rnf r
    {-# INLINE rnf #-}

rnfBS :: ByteString -> ()
rnfBS (Chunk _ xs) = rnfBS xs
rnfBS Empty        = ()
{-# INLINE rnfBS #-}

instance Show r => Show (Result r) where
    show (Fail bs stk msg) =
        "Fail " ++ show bs ++ " " ++ show stk ++ " " ++ show msg
    show (Done bs r)       = "Done " ++ show bs ++ " " ++ show r

fmapR :: (a -> b) -> Result a -> Result b
fmapR _ (Fail st stk msg) = Fail st stk msg
fmapR f (Done bs r)       = Done bs (f r)

instance Functor Result where
    fmap = fmapR

class A.Directed d => LazyDirected d where
  orderChunks :: Tagged d ByteString -> ByteString

instance LazyDirected A.Forward where
  orderChunks = untag
  {-# INLINE orderChunks #-}

instance LazyDirected A.Backward where
  {-# INLINE orderChunks #-}
  orderChunks = $(tw' "/") . go Nothing . untag
    where
      go Nothing Empty = Empty
      go (Just x) Empty = x
      go Nothing (Chunk x xs) = go (Just (Chunk x Empty)) xs
      go (Just p) (Chunk x xs) = go (Just (Chunk x p)) xs

type LbsParserCon d =
  ( I.BsParserCon d
  , LazyDirected d
  , I.DefaultDrift d
  )


-- | Run a parser and return its result.
dirParse :: forall a d. LbsParserCon d => A.DirParser d a -> ByteString -> Result a
dirParse p s = case orderChunks $ Tagged @d s of
              Chunk x xs -> go (A.dirParse p x) $ $(tr "/x xs") xs
              empty      -> go (A.dirParse p B.empty) $ $(tr "empty chunk") empty
  where
    go (T.Fail x stk msg) ys      = Fail (chunk x ys) stk msg
    go (T.Done x r) ys            = Done (chunk x ys) r
    go (T.Partial k) (Chunk y ys) = go (k y) ys
    go (T.Partial k) empty        = go (k B.empty) empty

parse :: A.Parser a -> ByteString -> Result a
parse = dirParse

parseBack :: A.BackParser a -> ByteString -> Result a
parseBack = dirParse

-- | Run a parser and print its result to standard output.
dirParseTest :: (LbsParserCon d, Show a) => A.DirParser d a -> ByteString -> IO ()
dirParseTest p s = print (dirParse p s)

parseTest :: (Show a) => A.Parser a -> ByteString -> IO ()
parseTest = dirParseTest

parseTestBack :: (Show a) => A.BackParser a -> ByteString -> IO ()
parseTestBack = dirParseTest

-- | Convert a 'Result' value to a 'Maybe' value.
maybeResult :: Result r -> Maybe r
maybeResult (Done _ r) = Just r
maybeResult _          = Nothing

-- | Convert a 'Result' value to an 'Either' value.
eitherResult :: Result r -> Either String r
eitherResult (Done _ r)        = Right r
eitherResult (Fail _ [] msg)   = Left msg
eitherResult (Fail _ ctxs msg) = Left (intercalate " > " ctxs ++ ": " ++ msg)

-- | Run a parser that cannot be resupplied via a 'T.Partial' result.
--
-- This function does not force a parser to consume all of its input.
-- Instead, any residual input will be discarded.  To force a parser
-- to consume all of its input, use something like this:
--
-- @
--'parseOnly' (myParser 'Control.Applicative.<*' 'endOfInput')
-- @
dirParseOnly :: LbsParserCon d => A.DirParser d a -> ByteString -> Either String a
dirParseOnly p = eitherResult . dirParse p
{-# INLINE dirParseOnly #-}

parseOnly :: A.Parser a -> ByteString -> Either String a
parseOnly = dirParseOnly
{-# INLINE parseOnly #-}

parseOnlyBack :: A.BackParser a -> ByteString -> Either String a
parseOnlyBack = dirParseOnly
{-# INLINE parseOnlyBack #-}
