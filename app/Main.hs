module Main where

import TurboM (Item, ItemsCollection, reviewItem)

-- Type alias for the specific instance where question and answer are Strings
type StringItem = Item String String

type StringItemsCollection = ItemsCollection String String

-- mem simple
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
