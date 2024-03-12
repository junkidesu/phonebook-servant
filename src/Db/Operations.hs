{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db.Operations where

import qualified Data.ByteString.UTF8 as BSU
import Data.Password.Bcrypt
import Database.PostgreSQL.Simple
import Db.Model.Person (Person)
import Db.Model.User (User)
import Db.Queries
import qualified Dto.EditPerson as EP
import qualified Dto.NewPerson as NP
import qualified Dto.NewUser as NU
import System.Environment (getEnv)

connectToDb :: IO Connection
connectToDb = getEnv "DATABASE_URL" >>= (connectPostgreSQL . BSU.fromString)

insertUser :: Connection -> NU.NewUser -> IO User
insertUser conn nu = do
  let pw = mkPassword (NU.password nu)
  hashedPw <- hashPassword pw
  [user] <- query conn insertUserQ (NU.username nu, unPasswordHash hashedPw)
  return user

allUsers :: Connection -> IO [User]
allUsers conn = query_ conn allUsersQ

deleteUser :: Connection -> Int -> IO ()
deleteUser conn userId = do
  _ <- execute conn deleteUserQ (Only userId)
  return ()

peopleInDB :: Connection -> IO [Person]
peopleInDB conn = query_ conn allPersonsQ

personById :: Connection -> Int -> IO (Maybe Person)
personById conn personId = do
  people <- query conn personByIdQ (Only personId) :: IO [Person]

  case people of
    [] -> return Nothing
    (x : _) -> return . Just $ x

insertPerson :: Connection -> Int -> NP.NewPerson -> IO Person
insertPerson conn userId np = do
  [person] <- query conn insertPersonQ (NP.name np, NP.number np, userId)
  return person

updateNumberInDB :: Connection -> Int -> EP.EditPerson -> IO (Maybe Person)
updateNumberInDB conn personId up = do
  _ <- execute conn updateNumberQ (EP.name up, EP.number up, personId)
  personById conn personId

deletePersonFromDB :: Connection -> Int -> IO ()
deletePersonFromDB conn personId = do
  _ <- execute conn deletePersonQ (Only personId)
  return ()
