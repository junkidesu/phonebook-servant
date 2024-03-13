{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db.Queries where

import Database.PostgreSQL.Simple

allUsersQ :: Query
allUsersQ = "SELECT * FROM users"

userByUsernameQ :: Query
userByUsernameQ = "SELECT * FROM users WHERE username = ?"

insertUserQ :: Query
insertUserQ = "INSERT INTO users (username, passwordHash) VALUES (?, ?) RETURNING id, username, passwordHash"

deleteUserQ :: Query
deleteUserQ = "DELETE FROM users WHERE id = ?"

allPersonsQ :: Query
allPersonsQ =
  "SELECT p.id, p.name, p.number, u.id, u.username, u.passwordHash FROM persons p "
    <> "JOIN users u "
    <> "ON p.author = u.id"

insertPersonQ :: Query
insertPersonQ =
  "WITH inserted_person AS ("
    <> "INSERT INTO persons (name, number, author) VALUES (?, ?, ?) "
    <> "RETURNING *"
    <> ") "
    <> "SELECT ip.id, ip.name, ip.number, u.id, u.username, u.passwordHash FROM inserted_person ip "
    <> "JOIN users u ON ip.author = u.id"

deletePersonQ :: Query
deletePersonQ = "DELETE FROM persons WHERE id = ?"

updateNumberQ :: Query
updateNumberQ =
  "WITH updated_person AS ("
    <> "UPDATE persons SET name = ?, number = ? WHERE id = ? "
    <> "RETURNING *)"
    <> "SELECT up.id, up.name, up.number, u.id, u.username, u.passwordHash FROM updated_person up "
    <> "JOIN users u ON up.author = u.id"

personByIdQ :: Query
personByIdQ = "SELECT * FROM persons WHERE id = ?"

personByNameQ :: Query
personByNameQ = "SELECT * FROM persons WHERE name = ?"
