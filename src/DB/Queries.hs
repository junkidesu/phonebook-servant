{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module DB.Queries where

import Database.PostgreSQL.Simple

initDBQuery :: Query
initDBQuery =
  "CREATE TABLE IF NOT EXISTS persons ("
    <> "id SERIAL PRIMARY KEY,"
    <> "name TEXT NOT NULL UNIQUE,"
    <> "number TEXT)"

allPersonsQuery :: Query
allPersonsQuery = "SELECT * FROM persons"

insertPersonQuery :: Query
insertPersonQuery = "INSERT INTO persons (name, number) VALUES (?, ?) RETURNING id, name, number"

deletePersonQuery :: Query
deletePersonQuery = "DELETE FROM persons WHERE id = ?"

updateNumberQuery :: Query
updateNumberQuery = "UPDATE persons SET number = ? WHERE id = ?"

personByIdQuery :: Query
personByIdQuery = "SELECT * FROM persons WHERE id = ?"

personByNameQuery :: Query
personByNameQuery = "SELECT * FROM persons WHERE name = ?"
