{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DB where

import DTO (NewPersonDTO (..))
import Database.SQLite.Simple
import Model (Person)

initDBQuery :: Query
initDBQuery =
  "CREATE TABLE IF NOT EXISTS Persons ("
    <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    <> "name TEXT NOT NULL UNIQUE,"
    <> "number TEXT)"

openDB :: IO Connection
openDB = do
  db <- open "dev.db"
  execute_ db initDBQuery
  return db

allPersonsQuery :: Query
allPersonsQuery = "SELECT * FROM Persons"

insertPersonQuery :: Query
insertPersonQuery = "INSERT INTO Persons (name, number) VALUES (?, ?)"

deletePersonQuery :: Query
deletePersonQuery = "DELETE FROM Persons WHERE id = ?"

personByNameQuery :: Query
personByNameQuery = "SELECT * FROM Persons WHERE name = ?"

peopleInDB :: Connection -> IO [Person]
peopleInDB db = query_ db allPersonsQuery

insertPersonInDB :: Connection -> NewPersonDTO -> IO Person
insertPersonInDB db np = do
  execute db insertPersonQuery np

  [person] <- query db personByNameQuery (Only . newName $ np) :: IO [Person]

  return person

deletePersonFromDB :: Connection -> Int -> IO ()
deletePersonFromDB db personId = execute db deletePersonQuery (Only personId)
