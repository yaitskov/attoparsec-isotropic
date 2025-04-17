-- | Root module to be imported by applications.
-- Parser combinators usually are omnidirected and direction is defined
-- via a top runner function:
--
-- +-------------------------+-------------------------+
-- | Left-to-Right (classic) | Right-to-Left (reverse) |
-- +=========================+=========================+
-- | 'parse'                 | 'parseBack'             |
-- +-------------------------+-------------------------+
-- | 'parseOnly'             | 'parseOnlyBack'         |
-- +-------------------------+-------------------------+
--
{-# LANGUAGE TemplateHaskell #-}
module Data.Attoparsec (module Data.Attoparsec.ByteString) where
import Haddock.UseRefs
import Data.Attoparsec.ByteString.Lazy (parseOnlyBack)
import Data.Attoparsec.ByteString

countDocRefs
