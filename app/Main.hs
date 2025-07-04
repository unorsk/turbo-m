{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Data.Foldable (traverse_)
import Data.List.Split (splitOn)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.Environment (getEnv)
import System.Process (spawnCommand)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import TurboM.Types (Item(..), FSRSRating(..), question, answer, stripSpacesToLowerCase)
import TurboM.Database
import TurboM.FSRS
import System.Random.Shuffle (shuffleM)
import Control.Monad.IO.Class (liftIO)
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
  
  case opts of
    -- List categories
    Options _ _ _ True -> do
      categories <- getCategories
      if null categories
        then putStrLn "No categories found. Import some vocabulary first."
        else do
          putStrLn "Available categories:"
          mapM_ (putStrLn . ("  " ++)) categories
    
    -- Import file
    Options (Just category) (Just filePath) _ _ -> do
      importVocabulary filePath category
      putStrLn $ "Imported vocabulary from " ++ filePath ++ " to category " ++ category
    
    -- Import without category
    Options Nothing (Just _) _ _ -> do
      putStrLn "Error: --import requires --category to be specified"
    
    -- Study mode
    Options (Just category) Nothing troubleWords _ -> do
      studyCategory category troubleWords
    
    -- No category specified
    Options Nothing Nothing _ _ -> do
      putStrLn "Error: Please specify a category with --category or use --list-categories"
      putStrLn "Use --help for more information"

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
studyCategory :: String -> Bool -> IO ()
studyCategory category troubleWords = do
  items <- if troubleWords 
           then getTroubleWords category
           else getDueItems category
  
  if null items
    then putStrLn $ "No items to study in category " ++ category
    else do
      fsrsParams <- getFSRSParameters
      shuffledItems <- shuffleM items
      let sortedItems = if troubleWords then sortByDifficulty shuffledItems else shuffledItems
      putStrLn $ "Category: " ++ category
      putStrLn $ "Items to study: " ++ show (length sortedItems)
      runInputT defaultSettings $ traverse_ (studyItem fsrsParams) sortedItems

-- Study a single item
studyItem :: FSRSParameters -> StringItem -> Repl ()
studyItem fsrsParams item = do
  rating <- getRatingFromUser item
  liftIO $ do
    -- Update item with FSRS
    updatedItem <- reviewItemFSRS fsrsParams item rating
    -- Save to database
    updateItemAfterReview updatedItem
    -- Log the review
    logReview updatedItem rating
    -- Say the answer
    sayText (answer item)
  return ()

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
        then do
          outputStrLn "Correct!"
          return Easy
        else if distance <= 2 -- Allow for small typos
          then do
            outputStrLn $ "Typo? Correct answer: " ++ answer item
            return Good
          else do
            outputStrLn $ "Incorrect. Correct answer: " ++ answer item
            return Hard



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



