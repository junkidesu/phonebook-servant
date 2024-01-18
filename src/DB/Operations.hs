{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DB.Operations where

import DB.Queries
import DTO (NewPersonDTO (..), UpdatePersonDTO (UpdatePersonDTO))
import Database.SQLite.Simple
import Model (Person)

openDB :: IO Connection
openDB = do
  db <- open "dev.db"
  execute_ db initDBQuery
  return db

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

updateNumberInDB :: Connection -> Int -> UpdatePersonDTO -> IO (Maybe Person)
updateNumberInDB db personId (UpdatePersonDTO number) = do
  execute db updateNumberQuery (number, personId)
  personById db personId

deletePersonFromDB :: Connection -> Int -> IO ()
deletePersonFromDB db personId = execute db deletePersonQuery (Only personId)