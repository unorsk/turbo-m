{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Data.List.Split (splitOn)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.Environment (getEnv)
import System.Process (spawnCommand, callCommand)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import TurboM.Types (Item(..), FSRSRating(..), question, answer, stripSpacesToLowerCase)
import TurboM.Database
import TurboM.FSRS
import System.Random.Shuffle (shuffleM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Options.Applicative

type Repl a = InputT IO a

-- Type alias for the specific instance where question and answer are Strings
type StringItem = Item String String

-- Command line options
data Options = Options
  { optCategory :: Maybe String
  , optImport :: Maybe String
  , optTroubleWords :: Bool
  , optListCategories :: Bool
  , optLimit :: Int
  } deriving (Show)

-- Command line parser
options :: Parser Options
options = Options
  <$> optional (strOption
      ( long "category"
     <> short 'c'
     <> metavar "CATEGORY"
     <> help "Category to study (e.g., DE, ES, EN)" ))
  <*> optional (strOption
      ( long "import"
     <> short 'i'
     <> metavar "FILE"
     <> help "Import vocabulary file" ))
  <*> switch
      ( long "trouble-words"
     <> short 't'
     <> help "Focus on trouble words" )
  <*> switch
      ( long "list-categories"
     <> short 'l'
     <> help "List available categories" )
  <*> option auto
      ( long "limit"
     <> short 'n'
     <> metavar "N"
     <> value 12
     <> showDefault
     <> help "Limit the number of items in a study session" )

parserInfo :: ParserInfo Options
parserInfo = info (options <**> helper)
  ( fullDesc
 <> progDesc "Turbo-M: FSRS-based spaced repetition learning"
 <> header "turbo-m - learn vocabulary with spaced repetition" )

-- Main entry point
main :: IO ()
main = do
  opts <- execParser parserInfo
  initializeDatabase
  
  case optCategory opts of
    Just category ->
      case optImport opts of
        Just filePath -> do
          importVocabulary filePath category
          putStrLn $ "Imported vocabulary from " ++ filePath ++ " to category " ++ category
        Nothing ->
          studyCategory category (optTroubleWords opts) (optLimit opts)
    Nothing ->
      if optListCategories opts
        then do
          categories <- getCategories
          if null categories
            then putStrLn "No categories found. Import some vocabulary first."
            else do
              putStrLn "Available categories:"
              mapM_ (putStrLn . ("  " ++)) categories
        else
          putStrLn "Error: Please specify a category with --category or use --list-categories"

-- Import vocabulary from file
importVocabulary :: String -> String -> IO ()
importVocabulary filePath category = do
  fileContents <- readFile filePath
  let vocabLines = lines fileContents
  mapM_ (importLine category) vocabLines
  where
    importLine :: String -> String -> IO ()
    importLine cat line = do
      let parts = splitOn "##" line
      case parts of
        [answerPart, questionPart] -> do
          let item = createNewItem questionPart answerPart cat
          insertItem item
        _ -> return () -- Skip malformed lines

-- Study a category
studyCategory :: String -> Bool -> Int -> IO ()
studyCategory category troubleWords limit = do
  items <- if troubleWords 
           then getTroubleWords category
           else getDueItems category
  
  if null items
    then putStrLn $ "No items to study in category " ++ category
    else do
      fsrsParams <- getFSRSParameters
      shuffledItems <- shuffleM items
      let sessionItems = take limit shuffledItems
          totalSessionItems = length sessionItems
          initialCorrectnessMap = Map.fromList $ map (\i -> (itemId i, 0)) sessionItems
      
      -- Clear the screen initially before starting the session
      callCommand "clear"
      
      runInputT defaultSettings $ studyLoop fsrsParams totalSessionItems Nothing sessionItems initialCorrectnessMap

-- The main study loop
studyLoop :: FSRSParameters -> Int -> Maybe (StringItem, FSRSRating) -> [StringItem] -> Map.Map (Maybe Int) Int -> Repl ()
studyLoop _ _ _ [] _ = do
  liftIO $ callCommand "clear"
  outputStrLn "Session complete! Well done."
studyLoop fsrsParams totalItems previousResult (item:restOfQueue) correctnessMap = do
  -- Clear screen and draw UI
  liftIO $ callCommand "clear"
  let masteredCount = Map.size $ Map.filter (>= 2) correctnessMap
  liftIO $ drawProgressBar totalItems masteredCount
  
  -- Show feedback from the previous turn, if it exists
  case previousResult of
    Just (prevItem, prevRating) -> liftIO $ displayPreviousFeedback prevRating prevItem
    Nothing -> outputStrLn "" -- Start with a blank line for spacing
  
  rating <- getRatingFromUser item
  updatedItem <- liftIO $ reviewItemFSRS fsrsParams item rating

  -- Always update the database after every single review
  liftIO $ updateItemAfterReview updatedItem
  liftIO $ logReview updatedItem rating
  
  -- Provide immediate audio feedback
  liftIO $ sayText (answer item)
  
  let currentCorrectCount = fromMaybe 0 (Map.lookup (itemId updatedItem) correctnessMap)
      isCorrect = rating == Good || rating == Easy

  let (nextQueue, nextCorrectnessMap) =
        if isCorrect then
          let newCorrectCount = currentCorrectCount + 1
          in if newCorrectCount >= 2 then
               -- Mastered: remove from queue
               (restOfQueue, Map.insert (itemId updatedItem) newCorrectCount correctnessMap)
             else
               -- Correct but not mastered: move to back
               (restOfQueue ++ [updatedItem], Map.insert (itemId updatedItem) newCorrectCount correctnessMap)
        else
          -- Incorrect: reset count and move to back
          (restOfQueue ++ [updatedItem], Map.insert (itemId updatedItem) 0 correctnessMap)

  -- Loop with the result of this turn as the new 'previousResult'
  studyLoop fsrsParams totalItems (Just (updatedItem, rating)) nextQueue nextCorrectnessMap

-- Display feedback for the previous item, positioned below the progress bar
displayPreviousFeedback :: FSRSRating -> StringItem -> IO ()
displayPreviousFeedback rating item = do
  let feedbackText = case rating of
        Easy  -> "Correct: " ++ question item ++ " -> " ++ answer item
        Good  -> "Typo: " ++ question item ++ " -> " ++ answer item
        Hard  -> "Incorrect: " ++ question item ++ " -> " ++ answer item
        Again -> "Skipped: " ++ question item ++ " -> " ++ answer item
  putStrLn feedbackText
  putStrLn "" -- Add a blank line for spacing

-- Draw a progress bar
drawProgressBar :: Int -> Int -> IO ()
drawProgressBar total mastered = do
  let barWidth = 50
      progress = if total > 0 then fromIntegral mastered / fromIntegral total else 0
      filledWidth = round (progress * fromIntegral barWidth)
      emptyWidth = barWidth - filledWidth
      
      filledChars = replicate filledWidth '━'
      emptyChars = replicate emptyWidth '─'
      
      -- ANSI escape codes for colors
      blue = "\x1b[34m"
      grey = "\x1b[90m"
      reset = "\x1b[0m"
      
  putStrLn $ blue ++ filledChars ++ grey ++ emptyChars ++ reset




-- Get rating from user based on Levenshtein distance
getRatingFromUser :: StringItem -> Repl FSRSRating
getRatingFromUser item = do
  minput <- getInputLine $ question item ++ "~ "
  case minput of
    Nothing -> return Again -- User wants to skip
    Just "" -> return Again -- Empty input is 'Again'
    Just input -> do
      let normalizedInput = stripSpacesToLowerCase input
          normalizedAnswer = stripSpacesToLowerCase (answer item)
          distance = levenshteinDistance defaultEditCosts normalizedInput normalizedAnswer
      
      if distance == 0
        then return Easy
        else if distance <= 2 -- Allow for small typos
          then return Good
          else return Hard



leftPad :: Int -> String -> String
leftPad n s = replicate n ' ' ++ s

printError :: Int -> String -> String -> Repl ()
printError offset expected _actual = outputStrLn $ leftPad offset expected

sayText :: String -> IO ()
sayText text = do
  homeDir <- getEnv "HOME"
  let cacheDir = homeDir </> ".turbo-m" </> "cache"
      wavFile = cacheDir </> (text ++ ".wav")
  fileExists <- doesFileExist wavFile
  void $ spawnCommand $ if fileExists
    then "afplay " ++ "\"" ++ wavFile ++ "\""
    else "say -v Anna " ++ "\"" ++ text ++ "\""



