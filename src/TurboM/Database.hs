{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TurboM.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Time
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))

import TurboM.Types
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import TurboM.FSRS (FSRSParameters(..), defaultFSRSParameters)

-- Database path
getDatabasePath :: IO FilePath
getDatabasePath = do
  home <- getHomeDirectory
  let dir = home </> ".turbo-m"
  createDirectoryIfMissing True dir
  return $ dir </> "turbo-m.db"

-- Initialize database with schema
initializeDatabase :: IO ()
initializeDatabase = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  
  -- Create items table
  execute_ conn $ Query $ 
    "CREATE TABLE IF NOT EXISTS items (" <>
    "id INTEGER PRIMARY KEY AUTOINCREMENT, " <>
    "question TEXT NOT NULL, " <>
    "answer TEXT NOT NULL, " <>
    "category TEXT NOT NULL, " <>
    "difficulty REAL DEFAULT 5.0, " <>
    "stability REAL DEFAULT 1.0, " <>
    "due_date TEXT, " <>
    "elapsed_days INTEGER DEFAULT 0, " <>
    "scheduled_days INTEGER DEFAULT 0, " <>
    "reps INTEGER DEFAULT 0, " <>
    "lapses INTEGER DEFAULT 0, " <>
    "state TEXT DEFAULT 'New', " <>
    "last_review TEXT, " <>
    "created_at TEXT DEFAULT CURRENT_TIMESTAMP, " <>
    "UNIQUE(question, answer, category)" <>
    ")"
  
  -- Create review_logs table
  execute_ conn $ Query $
    "CREATE TABLE IF NOT EXISTS review_logs (" <>
    "id INTEGER PRIMARY KEY AUTOINCREMENT, " <>
    "item_id INTEGER NOT NULL, " <>
    "rating TEXT NOT NULL, " <>
    "state TEXT NOT NULL, " <>
    "difficulty REAL NOT NULL, " <>
    "stability REAL NOT NULL, " <>
    "elapsed_days INTEGER NOT NULL, " <>
    "review_time TEXT DEFAULT CURRENT_TIMESTAMP, " <>
    "FOREIGN KEY(item_id) REFERENCES items(id)" <>
    ")"
  
  -- Create fsrs_parameters table
  execute_ conn $ Query $
    "CREATE TABLE IF NOT EXISTS fsrs_parameters (" <>
    "id INTEGER PRIMARY KEY DEFAULT 1, " <>
    "weights TEXT NOT NULL, " <>
    "retention_rate REAL DEFAULT 0.9, " <>
    "maximum_interval INTEGER DEFAULT 36500, " <>
    "updated_at TEXT DEFAULT CURRENT_TIMESTAMP" <>
    ")"
  
  -- Insert default FSRS parameters if not exists
  execute_ conn $ Query $
    "INSERT OR IGNORE INTO fsrs_parameters (id, weights) VALUES (1, " <>
    "'[0.4, 0.6, 2.4, 5.8, 4.93, 0.94, 0.86, 0.01, 1.49, 0.14, 0.94, 2.18, 0.05, 0.34, 1.26, 0.29, 2.61, 0.0, 0.0, 0.0, 0.0]')"
  
  close conn

-- FromRow instance for Item
instance FromRow (Item String String) where
  fromRow = Item <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- ToRow instance for Item
instance ToRow (Item String String) where
  toRow item =
    [ toField (itemQuestion item)
    , toField (itemAnswer item)
    , toField (itemCategory item)
    , toField (itemDifficulty item)
    , toField (itemStability item)
    , toField (itemDueDate item)
    , toField (itemElapsedDays item)
    , toField (itemScheduledDays item)
    , toField (itemReps item)
    , toField (itemLapses item)
    , toField (itemState item)
    , toField (itemLastReview item)
    ]

-- Insert a new item
insertItem :: Item String String -> IO ()
insertItem item = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  execute conn
    "INSERT OR IGNORE INTO items (question, answer, category, difficulty, stability, due_date, elapsed_days, scheduled_days, reps, lapses, state, last_review) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    item
  close conn

-- Get items by category
getItemsByCategory :: String -> IO [Item String String]
getItemsByCategory category = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  rows <- query conn "SELECT id, question, answer, category, difficulty, stability, due_date, elapsed_days, scheduled_days, reps, lapses, state, last_review FROM items WHERE category = ?" (Only category)
  close conn
  return rows

-- Get all categories
getCategories :: IO [String]
getCategories = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  rows <- query_ conn "SELECT DISTINCT category FROM items ORDER BY category"
  close conn
  return $ map fromOnly rows

-- Update item after review
updateItemAfterReview :: Item String String -> IO ()
updateItemAfterReview item = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  execute conn 
    "UPDATE items SET difficulty = ?, stability = ?, due_date = ?, elapsed_days = ?, scheduled_days = ?, reps = ?, lapses = ?, state = ?, last_review = ? WHERE id = ?"
    ( itemDifficulty item
    , itemStability item
    , itemDueDate item
    , itemElapsedDays item
    , itemScheduledDays item
    , itemReps item
    , itemLapses item
    , itemState item
    , itemLastReview item
    , itemId item
    )
  close conn

-- Log a review
logReview :: Item String String -> FSRSRating -> IO ()
logReview item rating = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  now <- getCurrentTime
  case itemId item of
    Just i -> do
      execute conn 
        "INSERT INTO review_logs (item_id, rating, state, difficulty, stability, elapsed_days, review_time) VALUES (?, ?, ?, ?, ?, ?, ?)"
        (i, rating, itemState item, itemDifficulty item, itemStability item, itemElapsedDays item, now)
    Nothing -> return () -- Should not happen
  close conn

-- Get due items for a category
getDueItems :: String -> IO [Item String String]
getDueItems category = do
  now <- getCurrentTime
  dbPath <- getDatabasePath
  conn <- open dbPath
  rows <- query conn
    "SELECT id, question, answer, category, difficulty, stability, due_date, elapsed_days, scheduled_days, reps, lapses, state, last_review FROM items WHERE category = ? AND (due_date IS NULL OR due_date <= ?)"
    (category, now)
  close conn
  return rows

-- Get FSRS parameters from the database
getFSRSParameters :: IO FSRSParameters
getFSRSParameters = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  -- Fetch the first row of parameters
  rows <- query_ conn "SELECT weights, retention_rate, maximum_interval FROM fsrs_parameters LIMIT 1"
  close conn
  case rows of
    [(weightsJson, retention, maxInterval)] ->
      case decode (BSL.pack weightsJson) of
        Just weights -> return $ FSRSParameters weights retention maxInterval
        Nothing -> return defaultFSRSParameters -- Fallback
    _ -> return defaultFSRSParameters -- Fallback

-- Get trouble words (high lapses, low success rate)
getTroubleWords :: String -> IO [Item String String]
getTroubleWords category = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  rows <- query conn
    "SELECT id, question, answer, category, difficulty, stability, due_date, elapsed_days, scheduled_days, reps, lapses, state, last_review FROM items WHERE category = ? AND (lapses > 2 OR (reps > 0 AND CAST(lapses AS REAL) / CAST(reps AS REAL) > 0.5)) ORDER BY lapses DESC"
    (Only category)
  close conn
  return rows