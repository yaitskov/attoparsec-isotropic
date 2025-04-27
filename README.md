# Welcome to attoparsec-monoidal

A fork of [attoparsec](https://github.com/haskell/attoparsec) library
allows to define omnidirected parsers or parsers consuming input from
right-to-left. The library is highly backward compabitle with original
interface.  Idea to do the fork is inspired by the need to parse a CSV
file in
[robin-hood-profit](https://github.com/yaitskov/RobinHood-pr0fit) in
one go with "constant" memory footprint and rows in reverse
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

## Running parser in reverse incrementally

Snippet from the CSV parser app:

``` haskell
consumeFile :: Handle -> (RobinRow -> ProfitM ()) -> ProfitM ()
consumeFile h handleRow = do
  input <- readBlock h
  go Nothing input
  where
    go !loopDetector input = do
      iBlock <- gets (^. #currentBlock)
      if iBlock < 0 && input == mempty
        then pure ()
        else do
          parseBackWith (readBlock h) parseRow input >>= \case
            Fail _unconsumed ctx er -> do
              erpos <- liftIO $ hTell h
              fail $ "Failed to parse CSV file around " <> show erpos <> " byte; due: "
                <> show er <> "; context: " <> show ctx
            Partial _ -> fail "CSV file is partial"
            Done (unconsumed :: ByteString) (rawRow :: [ByteString]) -> do
              iBlock' <- gets (^. #currentBlock)
              if loopDetector == Just (unconsumed, iBlock')
                then
                  fail $ "Loop detected. Unconsumed input: " <> show unconsumed
                else do
                  trashCodes <- asks (^. #codesToSkip)
                  case parseRobinRow trashCodes rawRow of
                    Left e -> fail e
                    Right row -> do
                      forM_ row handleRow
                      go (Just (unconsumed, iBlock')) unconsumed

```
