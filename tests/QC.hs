{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import qualified QC.Buffer as Buffer
import qualified QC.ByteString as ByteString
import qualified QC.ByteStringRightToLeft as ByteStringRightToLeft
import qualified QC.Combinator as Combinator
import qualified QC.CombinatorRight as CombinatorRight
import qualified QC.MonoidalScientific as MonoidalScientific
import qualified QC.Simple as Simple
import qualified QC.Text as Text
import Test.Tasty (defaultMain, testGroup)

main = defaultMain tests

tests = testGroup "tests"
  [ testGroup "leftToRight"
    [ testGroup "bs" ByteString.tests
    , testGroup "buf" Buffer.tests
    , testGroup "combinator" Combinator.tests
    , testGroup "simple" Simple.tests
    , testGroup "text" Text.tests
    ]
  , testGroup "rightToLeft"
    [ testGroup "bs" ByteStringRightToLeft.tests
    , testGroup "combinator" CombinatorRight.tests
    ]
  , testGroup "monoidal"
    [ testGroup "scientific" MonoidalScientific.tests
    ]
  ]
