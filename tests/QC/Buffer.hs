module QC.Buffer (tests) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid(mconcat))
#endif
import QC.Common ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import qualified Data.Attoparsec.ByteString.Buffer as BB
import qualified Data.Attoparsec.Text.Buffer as BT
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Text as T
import qualified Data.Text.Unsafe as T

data BP t b = BP [t] !t !b
            deriving (Eq, Show)

type BPB = BP B.ByteString BB.Buffer
type BPT = BP T.Text BT.Buffer

instance Arbitrary BPB where
  arbitrary = do
    bss <- arbitrary
    return $! toBP BB.buffer bss

  shrink (BP bss _ _) = toBP BB.buffer <$> shrink bss

instance Arbitrary BPT where
  arbitrary = do
    bss <- arbitrary
    return $! toBP BT.buffer bss

  shrink (BP bss _ _) = toBP BT.buffer <$> shrink bss

toBP :: (Monoid a, Monoid b) => (a -> b) -> [a] -> BP a b
toBP buf bss = BP bss (mconcat bss) (mconcat (map buf bss))

b_unbuffer :: BPB -> Property
b_unbuffer (BP _ts t buf) = t === BB.unbuffer buf

t_unbuffer :: BPT -> Property
t_unbuffer (BP _ts t buf) = t === BT.unbuffer buf

-- This test triggers both branches in Data.Attoparsec.Text.Buffer.append
-- and checks that Data.Text.Array.copyI manipulations are correct.
t_unbuffer_three :: Property
t_unbuffer_three = t_unbuffer $ toBP BT.buffer [t, t, t]
  where
    -- Make it long enough to increase chances of a segmentation fault
    t = T.replicate 1000 "\0"

b_length :: BPB -> Property
b_length (BP _ts t buf) = B.length t === BB.length buf

t_length :: BPT -> Property
t_length (BP _ts t buf) = BT.lengthCodeUnits t === BT.length buf

b_unsafeIndex :: BPB -> Gen Property
b_unsafeIndex (BP _ts t buf) = do
  let l = B.length t
  i <- choose (0,l-1)
  return $ l === 0 .||. B.unsafeIndex t i === BB.unsafeIndex buf i

t_iter :: BPT -> Gen Property
t_iter (BP _ts t buf) = do
  let l = BT.lengthCodeUnits t
  i <- choose (0,l-1)
  let it (T.Iter c q) = (c,q)
  return $ l === 0 .||. it (T.iter t i) === it (BT.iter buf i)

t_iter_ :: BPT -> Gen Property
t_iter_ (BP _ts t buf) = do
  let l = BT.lengthCodeUnits t
  i <- choose (0,l-1)
  return $ l === 0 .||. T.iter_ t i === BT.iter_ buf i

b_unsafeDrop :: BPB -> Gen Property
b_unsafeDrop (BP _ts t buf) = do
  i <- choose (0, B.length t)
  return $ B.unsafeDrop i t === BB.unsafeDrop i buf

t_dropCodeUnits :: BPT -> Gen Property
t_dropCodeUnits (BP _ts t buf) = do
  i <- choose (0, BT.lengthCodeUnits t)
  return $ dropCodeUnits i t === BT.dropCodeUnits i buf
  where
#if MIN_VERSION_text(2,0,0)
    dropCodeUnits = T.dropWord8
#else
    dropCodeUnits = T.dropWord16
#endif

tests :: [TestTree]
tests = [
    testProperty "b_unbuffer" b_unbuffer
  , testProperty "t_unbuffer" t_unbuffer
  , testProperty "t_unbuffer_three" t_unbuffer_three
  , testProperty "b_length" b_length
  , testProperty "t_length" t_length
  , testProperty "b_unsafeIndex" b_unsafeIndex
  , testProperty "t_iter" t_iter
  , testProperty "t_iter_" t_iter_
  , testProperty "b_unsafeDrop" b_unsafeDrop
  , testProperty "t_dropCodeUnits" t_dropCodeUnits
  ]
