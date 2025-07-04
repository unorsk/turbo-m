module TurboM.Types (Item (..), FSRSRating (..), CardState (..), stripSpacesToLowerCase, question, answer) where

import Data.Char (isSpace, toLower)
import Data.Time
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField

data Item q a = Item
  { itemId :: Maybe Int
  , itemQuestion :: q
  , itemAnswer :: a
  , itemCategory :: String
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





-- Helper function to get question from Item
question :: Item q a -> q
question = itemQuestion

-- Helper function to get answer from Item
answer :: Item q a -> a
answer = itemAnswer

-- FSRS Rating system
data FSRSRating = Again | Hard | Good | Easy deriving (Show, Eq, Enum)

-- FSRS card state
data CardState = New | Learning | Review | Relearning deriving (Show, Eq)

-- ToField instances for database storage
instance ToField FSRSRating where
  toField Again = toField ("Again" :: String)
  toField Hard = toField ("Hard" :: String)
  toField Good = toField ("Good" :: String)
  toField Easy = toField ("Easy" :: String)

instance FromField FSRSRating where
  fromField f = do
    s <- fromField f
    case s of
      "Again" -> return Again
      "Hard" -> return Hard
      "Good" -> return Good
      "Easy" -> return Easy
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
