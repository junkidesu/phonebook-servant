{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Phonebook.Database (PhonebookDb (..), db, connectToDb) where

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.Beam
import Database.Beam.Postgres
import Phonebook.Users.Database.Table (UserT, users)
import System.Environment (getEnv)

data PhonebookDb f = PhonebookDb
  { phonebookUsers :: f (TableEntity UserT)
  }
  deriving (Generic, Database be)

db :: DatabaseSettings be PhonebookDb
db =
  defaultDbSettings
    `withDbModification` dbModification
      { phonebookUsers = setEntityName "users"
      }

connectToDb :: IO (Pool Connection)
connectToDb = do
  dbConnString <- BSU.fromString <$> getEnv "DATABASE_URL"
  newPool . setNumStripes (Just 2) $
    defaultPoolConfig
      (connectPostgreSQL dbConnString)
      close
      60
      10

resetDb :: Pool Connection -> IO ()
resetDb conns = withResource conns $ \conn -> do
  runBeamPostgres conn $ do
    deleteAllData

    insertInitialUsers
 where
  deleteAllData =
    runDelete $
      delete
        (phonebookUsers db)
        (const (val_ True))
  insertInitialUsers =
    runInsert $
      insert (phonebookUsers db) $
        insertExpressions users

foo :: IO ()
foo = do
  loadFile defaultConfig

  conns <- connectToDb

  resetDb conns
