{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db.Operations where

import Data.Password.Bcrypt
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple
import Db.Model.Person (Person)
import Db.Model.User (User)
import Db.Queries
import qualified Dto.EditPerson as EP
import qualified Dto.NewPerson as NP
import qualified Dto.NewUser as NU

insertUser :: Pool Connection -> NU.NewUser -> IO User
insertUser conns nu = do
  let pw = mkPassword (NU.password nu)
  hashedPw <- hashPassword pw
  [user] <- withResource conns $
    \conn -> query conn insertUserQ (NU.username nu, unPasswordHash hashedPw)
  return user

allUsers :: Pool Connection -> IO [User]
allUsers conns = withResource conns $ \conn -> query_ conn allUsersQ

deleteUser :: Pool Connection -> Int -> IO ()
deleteUser conns userId = do
  _ <- withResource conns $ \conn -> execute conn deleteUserQ (Only userId)
  return ()

peopleInDB :: Pool Connection -> IO [Person]
peopleInDB conns = withResource conns $ \conn -> query_ conn allPersonsQ

personById :: Pool Connection -> Int -> IO (Maybe Person)
personById conns personId = do
  people <- withResource conns $ \conn -> query conn personByIdQ (Only personId) :: IO [Person]

  case people of
    [] -> return Nothing
    (x : _) -> return . Just $ x

insertPerson :: Pool Connection -> Int -> NP.NewPerson -> IO Person
insertPerson conns userId np = do
  [person] <- withResource conns $ \conn -> query conn insertPersonQ (NP.name np, NP.number np, userId)
  return person

updateNumberInDB :: Pool Connection -> Int -> EP.EditPerson -> IO (Maybe Person)
updateNumberInDB conns personId up = do
  _ <- withResource conns $ \conn -> execute conn updateNumberQ (EP.name up, EP.number up, personId)
  personById conns personId

deletePersonFromDB :: Pool Connection -> Int -> IO ()
deletePersonFromDB conns personId = do
  _ <- withResource conns $ \conn -> execute conn deletePersonQ (Only personId)
  return ()
