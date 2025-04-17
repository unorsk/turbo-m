module YatsuhashiSR (Item (..), ItemsCollection (..), ReviewGrade (..), reviewItem) where

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
