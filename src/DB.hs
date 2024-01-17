{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DB where

import DTO (NewPersonDTO (..), UpdatePersonDTO (UpdatePersonDTO))
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

updateNumberQuery :: Query
updateNumberQuery = "UPDATE Persons SET number = ? WHERE id = ?"

personByIdQuery :: Query
personByIdQuery = "SELECT * FROM Persons WHERE id = ?"

personByNameQuery :: Query
personByNameQuery = "SELECT * FROM Persons WHERE name = ?"

peopleInDB :: Connection -> IO [Person]
peopleInDB db = query_ db allPersonsQuery

personById :: Connection -> Int -> IO (Maybe Person)
personById db personId = do
  people <- query db personByIdQuery (Only personId) :: IO [Person]

  case people of
    [] -> return Nothing
    (x : _) -> return . Just $ x

insertPersonInDB :: Connection -> NewPersonDTO -> IO Person
insertPersonInDB db np = do
  execute db insertPersonQuery np

  [person] <- query db personByNameQuery (Only . newName $ np) :: IO [Person]

  return person

updateNumberInDB :: Connection -> Int -> UpdatePersonDTO -> IO Person
updateNumberInDB db personId (UpdatePersonDTO number) = do
  execute db updateNumberQuery (number, personId)

  [person] <- query db personByIdQuery (Only personId) :: IO [Person]

  return person

deletePersonFromDB :: Connection -> Int -> IO ()
deletePersonFromDB db personId = execute db deletePersonQuery (Only personId)
