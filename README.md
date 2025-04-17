# Welcome to attoparsec-monoidal

A fork of [attoparsec](https://github.com/haskell/attoparsec) library
allows to define omnidirected parsers or parsers consuming input from
right-to-left. The library is highly backward compabitle with original
interface.  Idea to do the fork is inspired by the need to parse a CSV
file in one go with constant memory footprint and rows in reverse
chronological order.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import Data.Attoparsec.ByteString

test = parseOnly ab "ab" == parseBackOnly ba "ab"
  where
    ab = (,) <$> string "a" <*> string "b"
    ba = (,) <$> string "b" <*> string "a"

test2 = parseOnly ab "ab" == parseBackOnly ab "ba"
  where
    ab = string "a" >*< string "b"
```
