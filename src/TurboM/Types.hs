module TurboM.Types (Item (..), ItemsCollection (..), ReviewGrade (..), reviewItem, InputValidator (..), TextToSpeech(..), stripSpacesToLowerCase) where

import Data.Char (isSpace, toLower)
import System.Process (spawnCommand)
import Control.Monad (void)

data Item q a = Item
  { question :: q,
    answer :: a,
    easinessFactor :: Double,
    repetitions :: Int,
    interval :: Int,
    dueDate :: Maybe String -- Optional due date for the item
  }
  deriving (Show, Eq)

data ItemsCollection q a = ItemsCollection
  { collectionName :: String,
    items :: [Item q a]
  }
  deriving (Show, Eq)

-- Helper functions for string normalization
isCharInString :: Char -> String -> Bool
isCharInString _ [] = False
isCharInString c (x : xs)
  | c == x = True
  | otherwise = isCharInString c xs

stripSpacesToLowerCase :: String -> String
stripSpacesToLowerCase =
  map toLower
    . filter (not . (\c -> isSpace c || isCharInString c "!¡.,?¿:;-–'\"()"))

data HowToSay = MacSay |  GeminiSay

class TextToSpeech w h where
  say :: w -> h -> IO ()

instance TextToSpeech String HowToSay where
  say w _h = void $ spawnCommand $ "say -v Anna " ++ "\"" ++ w ++ "\""

-- Typeclass for input validation strategies
class InputValidator q a where
    validateInput :: q -> a -> Bool

-- Instance for String comparison with normalization
instance InputValidator String String where
  validateInput q a = stripSpacesToLowerCase q == stripSpacesToLowerCase a

-- Enum to represent review grades
data ReviewGrade = Again | Hard | Good | Easy deriving (Show, Eq)

-- Function to update the easiness factor based on the review grade
updateEasinessFactor :: Double -> ReviewGrade -> Double
updateEasinessFactor ef grade =
  let q = case grade of
        Again -> 0 :: Integer
        Hard -> 3
        Good -> 4
        Easy -> 5
      newEf = ef - 0.8 + 0.28 * fromIntegral q - 0.02 * fromIntegral q * fromIntegral q
   in max 1.3 newEf

-- Function to review an item and update its state
reviewItem :: Item q a -> ReviewGrade -> Item q a
reviewItem item grade =
  let newRepetitions = if grade == Again then 0 else repetitions item + 1
      newEasinessFactor = updateEasinessFactor (easinessFactor item) grade
      newInterval = case newRepetitions of
        0 -> 0
        1 -> 1
        2 -> 6
        _ -> round $ fromIntegral (interval item) * newEasinessFactor
   in item
        { repetitions = newRepetitions,
          easinessFactor = newEasinessFactor,
          interval = newInterval
        }
