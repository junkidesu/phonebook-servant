{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DB.Operations where

import DB.Model (Person)
import DB.Queries
import DTO
import qualified Data.ByteString.UTF8 as BSU
import Database.PostgreSQL.Simple
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

insertPersonInDB :: Connection -> NewPersonDTO -> IO Person
insertPersonInDB conn np = do
  [person] <- query conn insertPersonQuery np
  return person

updateNumberInDB :: Connection -> Int -> UpdatePersonDTO -> IO (Maybe Person)
updateNumberInDB db personId (UpdatePersonDTO number) = do
  _ <- execute db updateNumberQuery (number, personId)
  personById db personId

deletePersonFromDB :: Connection -> Int -> IO ()
deletePersonFromDB db personId = do
  _ <- execute db deletePersonQuery (Only personId)
  return ()
