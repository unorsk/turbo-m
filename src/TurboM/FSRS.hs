{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TurboM.FSRS where

import Data.Time
import TurboM.Types

-- FSRS default weights (21 parameters)
defaultWeights :: [Double]
defaultWeights = [0.4, 0.6, 2.4, 5.8, 4.93, 0.94, 0.86, 0.01, 1.49, 0.14, 0.94, 2.18, 0.05, 0.34, 1.26, 0.29, 2.61, 0.0, 0.0, 0.0, 0.0]

-- FSRS Parameters
data FSRSParameters = FSRSParameters
  { fsrsWeights :: [Double]
  , fsrsRetentionRate :: Double
  , fsrsMaximumInterval :: Int
  } deriving (Show, Eq)

defaultFSRSParameters :: FSRSParameters
defaultFSRSParameters = FSRSParameters
  { fsrsWeights = defaultWeights
  , fsrsRetentionRate = 0.9
  , fsrsMaximumInterval = 36500
  }

-- Calculate retrievability R(t,S) = (1 + t/(9*S))^(-1)
calculateRetrievability :: Int -> Double -> Double
calculateRetrievability elapsedDays stability = 
  (1 + fromIntegral elapsedDays / (9 * stability)) ** (-1)

-- Calculate initial difficulty based on rating
calculateInitialDifficulty :: [Double] -> FSRSRating -> Double
calculateInitialDifficulty weights rating = 
  let w4 = weights !! 4
  in case rating of
    Hard -> w4 + 1
    Easy -> w4 - 1

-- Calculate initial stability based on rating
calculateInitialStability :: [Double] -> FSRSRating -> Double
calculateInitialStability weights rating = 
  case rating of
    Hard -> weights !! 1  -- w[1] for Hard
    Easy -> weights !! 0  -- w[0] for Easy

-- Update difficulty after review
updateDifficulty :: [Double] -> Double -> FSRSRating -> Double
updateDifficulty weights currentDifficulty rating = 
  let w6 = weights !! 6
      w7 = weights !! 7
      delta = case rating of
        Hard -> 1
        Easy -> -1
      newDifficulty = currentDifficulty + w6 * delta + w7 * (fromIntegral (fromEnum rating) - 2)
  in max 1 (min 10 newDifficulty)

-- Update stability after successful review
updateStability :: [Double] -> Double -> Double -> Int -> Double -> Double
updateStability weights currentStability currentDifficulty _elapsedDays retrievability = 
  let w8 = weights !! 8
      w9 = weights !! 9
      w10 = weights !! 10
      factor = exp w8 * (11 - currentDifficulty) * (currentStability ** (-w9)) * (exp (w10 * (1 - retrievability)) - 1)
  in currentStability * (1 + factor)

-- Calculate next review interval
calculateInterval :: Double -> Double -> Int
calculateInterval stability retentionRate = 
  let interval = stability * (retentionRate ** (1/stability) - 1) / (log retentionRate)
  in max 1 (round interval)

-- Review an item with FSRS algorithm
reviewItemFSRS :: FSRSParameters -> Item String String -> FSRSRating -> IO (Item String String)
reviewItemFSRS params item rating = do
  now <- getCurrentTime
  
  let weights = fsrsWeights params
      retentionRate = fsrsRetentionRate params
      maxInterval = fsrsMaximumInterval params
      
      -- Calculate elapsed days since last review
      elapsedDays = maybe 0 (\lastReview -> 
        let diff = diffUTCTime now lastReview
        in max 0 (round (diff / (24 * 3600)))) (itemLastReview item)
      
      -- Update based on current state
      (newDifficulty, newStability, newState, newReps, newLapses) = 
        case itemState item of
          New -> 
            let initialDifficulty = calculateInitialDifficulty weights rating
                initialStability = calculateInitialStability weights rating
                newState' = case rating of
                  Hard -> Learning
                  Easy -> Review
                newReps' = 1
                newLapses' = case rating of
                  Hard -> 1
                  Easy -> 0
            in (initialDifficulty, initialStability, newState', newReps', newLapses')
          
          Learning ->
            let currentRetrievability = calculateRetrievability elapsedDays (itemStability item)
                newDifficulty' = updateDifficulty weights (itemDifficulty item) rating
                newStability' = case rating of
                  Hard -> itemStability item * 0.5  -- Reduce stability on failure
                  Easy -> updateStability weights (itemStability item) (itemDifficulty item) elapsedDays currentRetrievability
                newState' = case rating of
                  Hard -> Learning  -- Stay in learning
                  Easy -> Review    -- Graduate to review
                newReps' = itemReps item + 1
                newLapses' = case rating of
                  Hard -> itemLapses item + 1
                  Easy -> itemLapses item
            in (newDifficulty', newStability', newState', newReps', newLapses')
          
          Review ->
            let currentRetrievability = calculateRetrievability elapsedDays (itemStability item)
                newDifficulty' = updateDifficulty weights (itemDifficulty item) rating
                newStability' = case rating of
                  Hard -> itemStability item * 0.5  -- Reduce stability on failure
                  Easy -> updateStability weights (itemStability item) (itemDifficulty item) elapsedDays currentRetrievability
                newState' = case rating of
                  Hard -> Relearning  -- Go to relearning
                  Easy -> Review      -- Stay in review
                newReps' = itemReps item + 1
                newLapses' = case rating of
                  Hard -> itemLapses item + 1
                  Easy -> itemLapses item
            in (newDifficulty', newStability', newState', newReps', newLapses')
          
          Relearning ->
            let currentRetrievability = calculateRetrievability elapsedDays (itemStability item)
                newDifficulty' = updateDifficulty weights (itemDifficulty item) rating
                newStability' = case rating of
                  Hard -> itemStability item * 0.5  -- Reduce stability on failure
                  Easy -> updateStability weights (itemStability item) (itemDifficulty item) elapsedDays currentRetrievability
                newState' = case rating of
                  Hard -> Relearning  -- Stay in relearning
                  Easy -> Review      -- Graduate back to review
                newReps' = itemReps item + 1
                newLapses' = case rating of
                  Hard -> itemLapses item + 1
                  Easy -> itemLapses item
            in (newDifficulty', newStability', newState', newReps', newLapses')
      
      -- Calculate next review interval
      interval = min maxInterval (calculateInterval newStability retentionRate)
      nextReviewTime = addUTCTime (fromIntegral interval * 24 * 3600) now
  
  return $ item
    { itemDifficulty = newDifficulty
    , itemStability = newStability
    , itemDueDate = Just nextReviewTime
    , itemElapsedDays = elapsedDays
    , itemScheduledDays = interval
    , itemReps = newReps
    , itemLapses = newLapses
    , itemState = newState
    , itemLastReview = Just now
    }

-- Create a new item with FSRS defaults
createNewItem :: String -> String -> Item String String
createNewItem question answer = Item
  { itemQuestion = question
  , itemAnswer = answer
  , itemDifficulty = 5.0  -- Default difficulty
  , itemStability = 1.0   -- Default stability
  , itemDueDate = Nothing
  , itemElapsedDays = 0
  , itemScheduledDays = 0
  , itemReps = 0
  , itemLapses = 0
  , itemState = New
  , itemLastReview = Nothing
  }

-- Sort items by difficulty (for trouble words)
sortByDifficulty :: [Item String String] -> [Item String String]
sortByDifficulty = sortBy compareDifficulty
  where
    compareDifficulty item1 item2 = 
      let score1 = calculateDifficultyScore item1
          score2 = calculateDifficultyScore item2
      in compare score2 score1  -- Higher difficulty first

-- Calculate difficulty score (combination of lapses and difficulty)
calculateDifficultyScore :: Item String String -> Double
calculateDifficultyScore item = 
  let lapseRatio = if itemReps item > 0 
                   then fromIntegral (itemLapses item) / fromIntegral (itemReps item)
                   else 0
  in itemDifficulty item * (1 + lapseRatio)

-- Import the sorting function
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:xs) = 
  let smaller = [y | y <- xs, cmp y x == LT]
      larger = [y | y <- xs, cmp y x /= LT]
  in sortBy cmp smaller ++ [x] ++ sortBy cmp larger