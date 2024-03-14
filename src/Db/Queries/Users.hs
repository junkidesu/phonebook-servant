{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db.Queries.Users where

import Database.PostgreSQL.Simple

allUsersQ :: Query
allUsersQ = "SELECT * FROM users"

userByUsernameQ :: Query
userByUsernameQ = "SELECT * FROM users WHERE username = ?"

insertUserQ :: Query
insertUserQ = "INSERT INTO users (username, passwordHash) VALUES (?, ?) RETURNING id, username, passwordHash"

deleteUserQ :: Query
deleteUserQ = "DELETE FROM users WHERE id = ?"
