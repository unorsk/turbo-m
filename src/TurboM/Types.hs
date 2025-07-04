module TurboM.Types (Item (..), ItemsCollection (..), FSRSRating (..), CardState (..), InputValidator (..), TextToSpeech(..), stripSpacesToLowerCase, question, answer) where

import Data.Char (isSpace, toLower)
import System.Process (spawnCommand)
import Control.Monad (void)
import Data.Time
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

data Item q a = Item
  { itemQuestion :: q
  , itemAnswer :: a
  , itemDifficulty :: Double        -- 1-10 scale
  , itemStability :: Double         -- days until 90% retrievability
  , itemDueDate :: Maybe UTCTime    -- next review date
  , itemElapsedDays :: Int         -- days since last review
  , itemScheduledDays :: Int       -- interval between reviews
  , itemReps :: Int                -- total review count
  , itemLapses :: Int              -- times forgotten
  , itemState :: CardState         -- current learning state
  , itemLastReview :: Maybe UTCTime -- last review timestamp
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

-- Helper function to get question from Item
question :: Item q a -> q
question = itemQuestion

-- Helper function to get answer from Item
answer :: Item q a -> a
answer = itemAnswer

-- FSRS Rating system (simplified to Easy/Hard initially)
data FSRSRating = Easy | Hard deriving (Show, Eq, Enum)

-- FSRS card state
data CardState = New | Learning | Review | Relearning deriving (Show, Eq)

-- ToField instances for database storage
instance ToField FSRSRating where
  toField Easy = toField ("Easy" :: String)
  toField Hard = toField ("Hard" :: String)

instance FromField FSRSRating where
  fromField f = do
    s <- fromField f
    case s of
      "Easy" -> return Easy
      "Hard" -> return Hard
      _ -> fail "Invalid FSRSRating"

instance ToField CardState where
  toField New = toField ("New" :: String)
  toField Learning = toField ("Learning" :: String)
  toField Review = toField ("Review" :: String)
  toField Relearning = toField ("Relearning" :: String)

instance FromField CardState where
  fromField f = do
    s <- fromField f
    case s of
      "New" -> return New
      "Learning" -> return Learning
      "Review" -> return Review
      "Relearning" -> return Relearning
      _ -> fail "Invalid CardState"
