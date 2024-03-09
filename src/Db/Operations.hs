{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db.Operations where

import qualified Data.ByteString.UTF8 as BSU
import Database.PostgreSQL.Simple
import Db.Model (Person)
import Db.Queries
import qualified Dto.EditPerson as EP
import qualified Dto.NewPerson as NP
import System.Environment (getEnv)

connectToDb :: IO Connection
connectToDb = do
  dbUrl <- getEnv "DATABASE_URL"
  conn <- connectPostgreSQL $ BSU.fromString dbUrl
  _ <- execute_ conn initDBQuery
  return conn

peopleInDB :: Connection -> IO [Person]
peopleInDB db = query_ db allPersonsQuery

personById :: Connection -> Int -> IO (Maybe Person)
personById db personId = do
  people <- query db personByIdQuery (Only personId) :: IO [Person]

  case people of
    [] -> return Nothing
    (x : _) -> return . Just $ x

insertPersonInDB :: Connection -> NP.NewPerson -> IO Person
insertPersonInDB conn np = do
  [person] <- query conn insertPersonQuery np
  return person

updateNumberInDB :: Connection -> Int -> EP.EditPerson -> IO (Maybe Person)
updateNumberInDB db personId up = do
  _ <- execute db updateNumberQuery (EP.name up, EP.number up, personId)
  personById db personId

deletePersonFromDB :: Connection -> Int -> IO ()
deletePersonFromDB db personId = do
  _ <- execute db deletePersonQuery (Only personId)
  return ()
