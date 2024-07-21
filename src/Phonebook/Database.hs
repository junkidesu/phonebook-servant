{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Phonebook.Database (PhonebookDb (..), db, connectToDb, resetDb) where

import Configuration.Dotenv (defaultConfig, loadFile)
import qualified Data.ByteString.UTF8 as BSU
import Data.Pool (Pool, defaultPoolConfig, newPool, setNumStripes, withResource)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning (runInsertReturningList))
import Database.Beam.Postgres
import Phonebook.Persons.Database.Table (PersonT (Person))
import Phonebook.Users.Database.Table (UserT, users)
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

resetDb :: Pool Connection -> IO ()
resetDb conns = withResource conns $ \conn -> do
  runBeamPostgresDebug putStrLn conn $ do
    deleteAllData

    [anwar, _, emily] <- insertInitialUsers

    let
      persons :: [PersonT (QExpr Postgres s)]
      persons =
        [ Person default_ (val_ "Junki") (val_ "12345678") (val_ $ pk anwar)
        , Person default_ (val_ "Martina") (val_ "23456789") (val_ $ pk anwar)
        , Person default_ (val_ "Weiwei") (val_ "34567890") (val_ $ pk emily)
        ]
    runInsert $
      insert (phonebookPersons db) $
        insertExpressions persons
 where
  deleteAllData =
    runDelete $
      delete
        (phonebookUsers db)
        (const (val_ True))
  insertInitialUsers =
    runInsertReturningList $
      insert (phonebookUsers db) $
        insertExpressions users

foo :: IO ()
foo = do
  loadFile defaultConfig

  conns <- connectToDb

  resetDb conns
