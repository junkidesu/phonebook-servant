{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Phonebook.Database (PhonebookDb (..), db, connectToDb, withDatabaseConnection) where

import Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.Beam
import Database.Beam.Postgres
import Phonebook.Persons.Database.Table (PersonT)
import Phonebook.Users.Database.Table (UserT)
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.Environment (Environment (connectionPool))
import System.Environment (getEnv)

data PhonebookDb f = PhonebookDb
  { phonebookUsers :: f (TableEntity UserT)
  , phonebookPersons :: f (TableEntity PersonT)
  }
  deriving (Generic, Database be)

db :: DatabaseSettings be PhonebookDb
db =
  defaultDbSettings
    `withDbModification` dbModification
      { phonebookUsers = setEntityName "users"
      , phonebookPersons = setEntityName "persons"
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

withDatabaseConnection :: (Connection -> IO b) -> AppM b
withDatabaseConnection f = do
  conns <- asks connectionPool

  liftIO $ withResource conns f
