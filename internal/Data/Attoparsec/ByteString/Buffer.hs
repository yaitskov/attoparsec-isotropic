
-- |
-- Module      :  Data.Attoparsec.ByteString.Buffer
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- An "immutable" buffer that supports cheap appends.
--
-- A Buffer is divided into an immutable read-only zone, followed by a
-- mutable area that we've preallocated, but not yet written to.
--
-- We overallocate at the end of a Buffer so that we can cheaply
-- append.  Since a user of an existing Buffer cannot see past the end
-- of its immutable zone into the data that will change during an
-- append, this is safe.
--
-- Once we run out of space at the end of a Buffer, we do the usual
-- doubling of the buffer size.
--
-- The fact of having a mutable buffer really helps with performance,
-- but it does have a consequence: if someone misuses the Partial API
-- that attoparsec uses by calling the same continuation repeatedly
-- (which never makes sense in practice), they could overwrite data.
--
-- Since the API *looks* pure, it should *act* pure, too, so we use
-- two generation counters (one mutable, one immutable) to track the
-- number of appends to a mutable buffer. If the counters ever get out
-- of sync, someone is appending twice to a mutable buffer, so we
-- duplicate the entire buffer in order to preserve the immutability
-- of its older self.
--
-- While we could go a step further and gain protection against API
-- abuse on a multicore system, by use of an atomic increment
-- instruction to bump the mutable generation counter, that would be
-- very expensive, and feels like it would also be in the realm of the
-- ridiculous.  Clients should never call a continuation more than
-- once; we lack a linear type system that could enforce this; and
-- there's only so far we should go to accommodate broken uses.

module Data.Attoparsec.ByteString.Buffer
    (
      Buffer
    , DirBuffer
    , Dir (..)
    , HasDrift (..)
    , DefaultDrift (..)
    , buffer
    , buffer'
    , unbuffer
    , pappend
    , pepreppend
    , length
    , unsafeIndex
    , substring
    , unsafeDrop
    ) where

import Control.Exception (assert)
import Data.ByteString.Internal (ByteString(..), nullForeignPtr)
import qualified Data.ByteString as B
import Data.Attoparsec.Internal.Fhthagn (inlinePerformIO)
import Data.Attoparsec.Internal.Compat ( mkPS, withPS )
import Data.List (foldl1')
import Data.Monoid as Mon (Monoid(..))
import Data.Proxy ( Proxy )
#if !MIN_VERSION_base(4,20,0)
import Data.Semigroup (Semigroup(..))
#endif
import Data.Word (Word8)
import Debug.TraceEmbrace ( tr, tw', ShowTrace(ShowTrace) )
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff, poke, sizeOf)
import GHC.ForeignPtr (mallocPlainForeignPtrBytes)
import Prelude hiding (length)

data Dir = Forward | Backward deriving (Eq, Show)
newtype Drift = Drift { unDrift :: Int } deriving (Eq, Show, Ord, Num)

type family DriftF (d :: Dir)
type instance DriftF Forward = ()
type instance DriftF Backward = Drift

-- If _cap is zero, this buffer is empty.
data DirBuffer (d :: Dir) = Buf {
      _fp  :: {-# UNPACK #-} !(ForeignPtr Word8)
    , _off :: {-# UNPACK #-} !Int
    , _len :: {-# UNPACK #-} !Int
    , _cap :: {-# UNPACK #-} !Int
    , _gen :: {-# UNPACK #-} !Int
    , _drift :: {- UNPACK  not working with type family -} !(DriftF d)
    }

class DefaultDrift (d :: Dir) where
  initDrift :: Proxy d -> DriftF d
instance DefaultDrift Forward where
  initDrift _ = ()
instance DefaultDrift Backward where
  initDrift _ = 0

class HasDrift (d :: Dir) where
  getDrift :: DirBuffer d -> Int

instance HasDrift Forward where
  getDrift _ = 0

instance HasDrift Backward where
  getDrift Buf {_drift} = unDrift _drift

type Buffer = DirBuffer Forward

instance Show (DirBuffer d) where
    showsPrec p = showsPrec p . unbuffer

instance Show (ShowTrace (DirBuffer Forward)) where
  show (ShowTrace (Buf fp0 off0 len0 cap0 gen0 ())) =
    "Buf -> fp0: " <> show fp0 <> "; off0: " <> show off0 <> "; len0: " <> show len0 <> "; cap0: "
        <> show cap0 <> "; gen0: " <> show gen0

instance Show (ShowTrace (DirBuffer Backward)) where
  show (ShowTrace (Buf fp0 off0 len0 cap0 gen0 drift)) =
    "Buf <- fp0: " <> show fp0 <> "; off0: " <> show off0 <> "; len0: " <> show len0 <> "; cap0: "
        <> show cap0 <> "; gen0: " <> show gen0 <> "; drift: " <> show drift

-- | The initial 'Buffer' has no mutable zone, so we can avoid all
-- copies in the (hopefully) common case of no further input being fed
-- to us.
buffer :: ByteString -> Buffer
buffer bs = withPS bs $ \fp off len ->
  Buf fp off len len ($(tr "make buffer/bs fp off len") 0) ()

buffer' :: DriftF d -> ByteString -> DirBuffer d
buffer' d bs = withPS bs $ \fp off len ->
  Buf fp off len len ($(tr "make buffer/bs fp off len") 0) d

unbuffer :: DirBuffer d -> ByteString
unbuffer (Buf fp off len _ _ _) = mkPS fp off len

instance Semigroup (DirBuffer Forward) where
    (Buf _ _ _ 0 _ ()) <> b                    = b
    a               <> (Buf _ _ _ 0 _ ())      = a
    buf             <> (Buf fp off len _ _ ()) = append buf fp off len

instance Semigroup (DirBuffer Backward) where
    (Buf _ _ _ 0 _ _) <> b                    = b
    a               <> (Buf _ _ _ 0 _ _)      = a
    (Buf fp off len _ _ _) <> buf  = preppend buf fp off len

instance Monoid (DirBuffer Forward) where
    mempty = Buf nullForeignPtr 0 0 0 0 ()
    mappend = (<>)
    mconcat [] = Mon.mempty
    mconcat xs = foldl1' mappend xs

instance Monoid (DirBuffer Backward) where
    mempty = Buf nullForeignPtr 0 0 0 0 0
    mappend = (<>)
    mconcat [] = Mon.mempty
    mconcat xs = foldl1' mappend xs

pappend :: Buffer -> ByteString -> Buffer
pappend (Buf _ _ _ 0 _ ()) bs  = buffer bs
pappend buf             bs  = withPS bs $ \fp off len -> append buf fp off len

append :: Buffer -> ForeignPtr a -> Int -> Int -> Buffer
append (Buf fp0 off0 len0 cap0 gen0 ()) !fp1 !off1 !len1 =
  inlinePerformIO . withForeignPtr fp0 $ \ptr0 ->
    withForeignPtr fp1 $ \ptr1 -> do
      let genSize = sizeOf (0::Int)
          newlen  = len0 + len1
      gen <- if gen0 == 0
             then return 0
             else peek (castPtr ptr0)
      if gen == gen0 && newlen <= cap0
        then do
          let newgen = gen + 1
          poke (castPtr ptr0) newgen
          copyBytes (ptr0 `plusPtr` (off0+len0))
                    (ptr1 `plusPtr` off1)
                    (fromIntegral len1)
          return (Buf fp0 off0 newlen cap0 newgen ())
        else do
          let newcap = newlen * 2
          fp <- mallocPlainForeignPtrBytes (newcap + genSize)
          withForeignPtr fp $ \ptr_ -> do
            let ptr    = ptr_ `plusPtr` genSize
                newgen = 1
            poke (castPtr ptr_) newgen
            copyBytes ptr (ptr0 `plusPtr` off0) (fromIntegral len0)
            copyBytes (ptr `plusPtr` len0) (ptr1 `plusPtr` off1)
                      (fromIntegral len1)
            return (Buf fp genSize newlen newcap newgen ())

pepreppend :: DirBuffer Backward -> ByteString -> DirBuffer Backward
pepreppend (Buf _ _ _ 0 _ drift) bs  =
  buffer' (drift + Drift (B.length bs)) $ $(tw' "pepreppend zero cap/drift") bs
pepreppend buf bs =
  withPS bs $ \fp off len -> preppend buf fp off len

preppend :: DirBuffer Backward -> ForeignPtr a -> Int -> Int -> DirBuffer Backward
preppend (Buf fp0 off0 len0 cap0 gen0 drift) !fp1 !off1 !len1 =
  inlinePerformIO . withForeignPtr fp0 $ \ptr0 ->
    withForeignPtr fp1 $ \ptr1 -> do
      let genSize = sizeOf (0::Int)
          newlen  = len0 + len1
      gen <- if gen0 == 0
             then return 0
             else peek (castPtr ptr0)
      if gen == gen0 && newlen <= cap0
        then do
          let newgen = gen + 1
          let newoff = off0 - len1
          poke (castPtr ptr0) newgen
          copyBytes (ptr0 `plusPtr` newoff)
                    (ptr1 `plusPtr` off1)
                    (fromIntegral len1)
          return (Buf fp0 newoff newlen cap0 newgen $ drift + Drift len1)
        else do
          let newcap = newlen * 2
          fp <- mallocPlainForeignPtrBytes (newcap + genSize)
          withForeignPtr fp $ \ptr_ -> do
            let ptr    = ptr_ `plusPtr` genSize
                newgen = 1
            poke (castPtr ptr_) newgen
            copyBytes (ptr `plusPtr` (newcap - newlen)) (ptr1 `plusPtr` off1) (fromIntegral len1)
            copyBytes (ptr `plusPtr` (newcap - len0)) (ptr0 `plusPtr` off0) (fromIntegral len0)
            return (Buf fp (genSize + newcap - newlen) newlen newcap newgen $ drift + Drift len1)

length :: DirBuffer d -> Int
length (Buf _ _ len _ _ _) = len
{-# INLINE length #-}

unsafeIndex :: DirBuffer d -> Int -> Word8
unsafeIndex (Buf fp off len _ _ _) i = assert (i >= 0 && i < len) .
    inlinePerformIO . withForeignPtr fp $ flip peekByteOff (off+i)
{-# INLINE unsafeIndex #-}

substring :: Int -> Int -> DirBuffer d -> ByteString
substring s l (Buf fp off len _ _ _) =
  assert (s >= 0 && s <= len) .
  assert (l >= 0 && l <= len-s) $
  mkPS fp (off+s) l
{-# INLINE substring #-}

unsafeDrop :: Int -> DirBuffer d -> ByteString
unsafeDrop s (Buf fp off len _ _ _) =
  assert (s >= 0 && s <= len) $
  mkPS fp (off+s) (len-s)
{-# INLINE unsafeDrop #-}
