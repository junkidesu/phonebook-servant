{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DB.Queries where

import Database.SQLite.Simple

initDBQuery :: Query
initDBQuery =
  "CREATE TABLE IF NOT EXISTS Persons ("
    <> "id INTEGER PRIMARY KEY AUTOINCREMENT,"
    <> "name TEXT NOT NULL UNIQUE,"
    <> "number TEXT)"

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