{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module TurboM.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Data.Time
import System.Directory (getHomeDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import TurboM.Types

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

-- Database item representation
data DbItem = DbItem
  { dbItemId :: Maybe Int
  , dbItemQuestion :: String
  , dbItemAnswer :: String
  , dbItemCategory :: String
  , dbItemDifficulty :: Double
  , dbItemStability :: Double
  , dbItemDueDate :: Maybe UTCTime
  , dbItemElapsedDays :: Int
  , dbItemScheduledDays :: Int
  , dbItemReps :: Int
  , dbItemLapses :: Int
  , dbItemState :: CardState
  , dbItemLastReview :: Maybe UTCTime
  } deriving (Show, Eq)

-- We don't need ToRow for DbItem since we'll construct queries manually

-- FromRow instance for DbItem
instance FromRow DbItem where
  fromRow = DbItem <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Convert DbItem to Item
dbItemToItem :: DbItem -> Item String String
dbItemToItem (DbItem _ q a _ d s dd ed sd r l st lr) = 
  Item q a d s dd ed sd r l st lr

-- Convert Item to DbItem
itemToDbItem :: String -> Item String String -> DbItem
itemToDbItem category (Item q a d s dd ed sd r l st lr) = 
  DbItem Nothing q a category d s dd ed sd r l st lr

-- Insert a new item
insertItem :: String -> Item String String -> IO ()
insertItem category item = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  execute conn 
    "INSERT OR IGNORE INTO items (question, answer, category, difficulty, stability, due_date, elapsed_days, scheduled_days, reps, lapses, state, last_review) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    [ toField (itemQuestion item)
    , toField (itemAnswer item)
    , toField category
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
  close conn

-- Get items by category
getItemsByCategory :: String -> IO [Item String String]
getItemsByCategory category = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  rows <- query conn "SELECT id, question, answer, category, difficulty, stability, due_date, elapsed_days, scheduled_days, reps, lapses, state, last_review FROM items WHERE category = ?" (Only category)
  close conn
  return $ map dbItemToItem rows

-- Get all categories
getCategories :: IO [String]
getCategories = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  rows <- query_ conn "SELECT DISTINCT category FROM items ORDER BY category"
  close conn
  return $ map fromOnly rows

-- Update item after review
updateItemAfterReview :: String -> Item String String -> IO ()
updateItemAfterReview category item = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  execute conn 
    "UPDATE items SET difficulty = ?, stability = ?, due_date = ?, elapsed_days = ?, scheduled_days = ?, reps = ?, lapses = ?, state = ?, last_review = ? WHERE question = ? AND answer = ? AND category = ?"
    [ toField (itemDifficulty item)
    , toField (itemStability item)
    , toField (itemDueDate item)
    , toField (itemElapsedDays item)
    , toField (itemScheduledDays item)
    , toField (itemReps item)
    , toField (itemLapses item)
    , toField (itemState item)
    , toField (itemLastReview item)
    , toField (itemQuestion item)
    , toField (itemAnswer item)
    , toField category
    ]
  close conn

-- Log a review
logReview :: String -> Item String String -> FSRSRating -> IO ()
logReview category item rating = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  -- First get the item ID
  rows <- query conn "SELECT id FROM items WHERE question = ? AND answer = ? AND category = ?" (itemQuestion item, itemAnswer item, category)
  case rows of
    [Only itemId] -> do
      now <- getCurrentTime
      execute conn 
        "INSERT INTO review_logs (item_id, rating, state, difficulty, stability, elapsed_days, review_time) VALUES (?, ?, ?, ?, ?, ?, ?)"
        (itemId :: Int, rating, itemState item, itemDifficulty item, itemStability item, itemElapsedDays item, now)
      return ()
    _ -> return () -- Item not found
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
  return $ map dbItemToItem rows

-- Get trouble words (high lapses, low success rate)
getTroubleWords :: String -> IO [Item String String]
getTroubleWords category = do
  dbPath <- getDatabasePath
  conn <- open dbPath
  rows <- query conn 
    "SELECT id, question, answer, category, difficulty, stability, due_date, elapsed_days, scheduled_days, reps, lapses, state, last_review FROM items WHERE category = ? AND (lapses > 2 OR (reps > 0 AND CAST(lapses AS REAL) / CAST(reps AS REAL) > 0.5)) ORDER BY lapses DESC"
    (Only category)
  close conn
  return $ map dbItemToItem rows