{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module API where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Model (Person)
import DB.Operations (
  deletePersonFromDB,
  insertPersonInDB,
  peopleInDB,
  personById,
  updateNumberInDB,
 )
import DTO (NewPersonDTO, UpdatePersonDTO)
import Database.PostgreSQL.Simple
import Servant

type GetAllPersons = Get '[JSON] [Person]

type AddPerson = ReqBody '[JSON] NewPersonDTO :> PostCreated '[JSON] Person

type GetPerson = Get '[JSON] Person

type DeletePerson = DeleteNoContent

type ChangeNumber = ReqBody '[JSON] UpdatePersonDTO :> Put '[JSON] Person

type PersonAPI =
  "persons"
    :> ( GetAllPersons
          :<|> AddPerson
          :<|> Capture "id" Int :> (GetPerson :<|> DeletePerson :<|> ChangeNumber)
       )

personsServer :: Connection -> Server PersonAPI
personsServer db =
  getAllPersons
    :<|> addPerson
    :<|> personOperations
 where
  getAllPersons :: Handler [Person]
  getAllPersons = liftIO . peopleInDB $ db

  addPerson :: NewPersonDTO -> Handler Person
  addPerson np = do
    res <- liftIO (try (insertPersonInDB db np) :: IO (Either SqlError Person))

    case res of
      Left _ -> throwError err400{errBody = "Couldn't add new person :("}
      Right p -> return p

  personOperations personId = getPerson :<|> deletePerson :<|> changeNumber
   where
    getPerson :: Handler Person
    getPerson = do
      person <- liftIO $ personById db personId

      case person of
        Nothing -> throwError err404{errBody = "Person not found :("}
        Just p -> return p

    deletePerson :: Handler NoContent
    deletePerson = do
      liftIO $ deletePersonFromDB db personId
      return NoContent

    changeNumber :: UpdatePersonDTO -> Handler Person
    changeNumber up = do
      res <- liftIO . updateNumberInDB db personId $ up

      case res of
        Nothing -> throwError err404{errBody = "Person not found :("}
        Just p -> return p
