{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Db.Operations.Persons where

import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple
import Db.Queries.Persons
import qualified Types.EditPerson as EP
import qualified Types.NewPerson as NP
import Types.Person (Person)

peopleInDB :: Pool Connection -> IO [Person]
peopleInDB conns = withResource conns $
  \conn -> query_ conn allPersonsQ

personById :: Pool Connection -> Int -> IO (Maybe Person)
personById conns personId = do
  people <- withResource conns $
    \conn -> query conn personByIdQ (Only personId) :: IO [Person]

  case people of
    [] -> return Nothing
    (x : _) -> return . Just $ x

insertPerson :: Pool Connection -> Int -> NP.NewPerson -> IO Person
insertPerson conns userId np = do
  [person] <- withResource conns $
    \conn -> query conn insertPersonQ (NP.name np, NP.number np, userId)
  return person

updateNumberInDB :: Pool Connection -> Int -> EP.EditPerson -> IO Person
updateNumberInDB conns personId up = do
  [person] <- withResource conns $
    \conn -> query conn updateNumberQ (EP.name up, EP.number up, personId) :: IO [Person]
  return person

deletePersonFromDB :: Pool Connection -> Int -> IO ()
deletePersonFromDB conns personId = do
  _ <- withResource conns $ \conn -> execute conn deletePersonQ (Only personId)
  return ()
