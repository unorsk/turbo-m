{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Main where

import Data.Foldable (traverse_)
import Data.List qualified
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import System.Environment (getArgs, getEnv)
import System.Process (spawnCommand)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import TurboM.Types (Item(..), ItemsCollection(..), InputValidator(..))
import System.Random.Shuffle (shuffleM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)

type Repl a = InputT IO a

-- Type alias for the specific instance where question and answer are Strings
type StringItem = Item String String
-- Should be a list of items
type StringItemsCollection = ItemsCollection String String

-- mem simple
main :: IO ()
main = do
  (path, _) <- fromMaybe (error "Error: No arguments provided. Please specify the training set path.") . Data.List.uncons <$> getArgs
  itms <- (readItemsFromFile path "##") >>= shuffleLearningItems
  putStrLn $ "File: " ++ path
  putStrLn $ "Rounds: " ++ show times <> " Items: " <> show (length $ items itms)
  doTraining itms times
  where
    times = 3

doTraining :: StringItemsCollection -> Int -> IO ()
doTraining _ 0 = return ()
doTraining itms times = do
  runInputT defaultSettings $ traverse_ readAndCheckWithSpeech (items itms)
  doTraining itms (times - 1)

readAndCheckWithSpeech :: StringItem -> Repl Bool
readAndCheckWithSpeech item = do
  result <- readAndCheck item
  liftIO $ sayText (answer item)
  return result

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

readAndCheck :: StringItem -> Repl Bool
readAndCheck item = do
  minput <- getInputLine $ question item ++ "~ "
  case minput of
    Nothing -> return False
    Just input ->
      let result = validateInput input $ answer item
       in do
            if not result
              then printError (length (question item) + 2) (answer item) input
              else return ()
            return result

shuffleLearningItems :: StringItemsCollection -> IO StringItemsCollection
shuffleLearningItems itemsCollection = do
  shuffledItems <- shuffleM $ items itemsCollection
  return $ ItemsCollection (collectionName itemsCollection) shuffledItems

readItemsFromFile :: String -> String -> IO StringItemsCollection
readItemsFromFile filePath separator = do
  fileContents <- readFile filePath
  let itms = map parseLine $ lines fileContents
  return $ ItemsCollection "DICT TOOD" itms
  where
    parseLine :: String -> StringItem
    parseLine line = (Item (last parts) (head parts) 0 0 0 Nothing) :: StringItem -- <- TODO
      where
        parts = splitOn separator line
