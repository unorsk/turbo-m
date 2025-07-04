{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TurboM.FSRS where

import Data.Time
import Data.List (sortBy)
import TurboM.Types

-- FSRS default weights (17 parameters for this implementation)
-- These are based on the standard FSRS-4.5 weights.
defaultWeights :: [Double]
defaultWeights = [0.4, 0.9, 2.3, 5.8, 4.93, 0.94, 0.86, 0.01, 1.49, 0.14, 0.94, 2.18, 0.05, 0.34, 1.26, 0.29, 2.61]

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

-- Grade from FSRSRating
ratingToGrade :: FSRSRating -> Double
ratingToGrade Again = 1
ratingToGrade Hard = 2
ratingToGrade Good = 3
ratingToGrade Easy = 4

-- Calculate retrievability: R = (1 + t / (9 * S)) ^ -1
calculateRetrievability :: Int -> Double -> Double
calculateRetrievability elapsedDays stability =
  (1 + fromIntegral elapsedDays / (9 * stability)) ** (-1)

-- Calculate new difficulty: D' = D - w6 * (g - 3)
updateDifficulty :: [Double] -> Double -> Double -> Double
updateDifficulty weights d g =
  let w6 = weights !! 6
  in max 1 (min 10 (d - w6 * (g - 3)))

-- Calculate new stability after a successful review (g > 1)
-- S' = S * (1 + exp(w7) * (11 - D) * S^(-w8) * (exp((1 - R) * w9) - 1))
updateStability :: [Double] -> Double -> Double -> Double -> Double
updateStability weights d s r =
  let w7 = weights !! 7
      w8 = weights !! 8
      w9 = weights !! 9
      factor = exp w7 * (11 - d) * (s ** (-w8)) * (exp ((1 - r) * w9) - 1)
  in s * (1 + factor)

-- Calculate new stability after a failed review (g = 1)
-- S' = w10 * D^(-w11) * S^(w12) * exp((1 - R) * w13)
updateStabilityAfterFail :: [Double] -> Double -> Double -> Double -> Double
updateStabilityAfterFail weights d s r =
    let w10 = weights !! 10
        w11 = weights !! 11
        w12 = weights !! 12
        w13 = weights !! 13
    in w10 * (d ** (-w11)) * (s ** w12) * exp ((1 - r) * w13)

-- Calculate next interval
calculateInterval :: Double -> Double -> Int
calculateInterval stability retention =
  round $ stability * log retention / log 0.9

-- Review an item with FSRS algorithm
reviewItemFSRS :: FSRSParameters -> Item String String -> FSRSRating -> IO (Item String String)
reviewItemFSRS params item rating = do
  now <- getCurrentTime

  let weights = fsrsWeights params
      retention = fsrsRetentionRate params
      maxInterval = fsrsMaximumInterval params
      grade = ratingToGrade rating

      elapsedDays = maybe 0 (\lastReview ->
        let diff = diffUTCTime now lastReview
        in max 0 (round (diff / (24 * 3600)))) (itemLastReview item)

      (currentStability, currentDifficulty) = (itemStability item, itemDifficulty item)
      retrievability = calculateRetrievability elapsedDays currentStability

      (newDifficulty, newStability, newLapses) = case itemState item of
        New ->
          let d0 = weights !! 4 - (grade - 3) * (weights !! 5)
              s0 = weights !! (round grade - 1)
              lapses' = if grade == 1 then 1 else 0
          in (d0, s0, lapses')
        _ -> -- Learning, Review, Relearning
          let d' = updateDifficulty weights currentDifficulty grade
              lapses' = if grade == 1 then itemLapses item + 1 else itemLapses item
          in if grade == 1 -- Again
               then (d', updateStabilityAfterFail weights d' currentStability retrievability, lapses')
               else (d', updateStability weights d' currentStability retrievability, lapses')

      newState = case itemState item of
        New -> if grade == 1 then Relearning else Review
        Learning -> if grade == 1 then Relearning else Review
        Review -> if grade == 1 then Relearning else Review
        Relearning -> if grade == 1 then Relearning else Review

      newReps = itemReps item + 1
      interval = min maxInterval (calculateInterval newStability retention)
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
createNewItem :: String -> String -> String -> Item String String
createNewItem question answer category = Item
  { itemId = Nothing
  , itemQuestion = question
  , itemAnswer = answer
  , itemCategory = category
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