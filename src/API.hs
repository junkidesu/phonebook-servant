{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API (PersonAPI, personsServer) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB.Operations
import DTO (NewPersonDTO, UpdatePersonDTO)
import Database.SQLite.Simple
import Model
import Servant

type GetAllPersons = Get '[JSON] [Person]

type AddPerson = ReqBody '[JSON] NewPersonDTO :> PostCreated '[JSON] Person

type GetPerson = Get '[JSON] Person

type DeletePerson = DeleteNoContent

type ChangeNumber = ReqBody '[JSON] UpdatePersonDTO :> Put '[JSON] Person

type PersonOperations = Capture "id" Int :> (GetPerson :<|> DeletePerson :<|> ChangeNumber)

type PersonAPI =
  "persons"
    :> ( GetAllPersons
           :<|> AddPerson
           :<|> PersonOperations
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
      res <- liftIO (try (insertPersonInDB db np) :: IO (Either SQLError Person))

      case res of
        Left _ -> throwError err400 {errBody = "Couldn't add new person :("}
        Right p -> return p

    personOperations personId = getPerson :<|> deletePerson :<|> changeNumber
      where
        getPerson :: Handler Person
        getPerson = do
          person <- liftIO $ personById db personId

          case person of
            Nothing -> throwError err404 {errBody = "Person not found :("}
            Just p -> return p

        deletePerson :: Handler NoContent
        deletePerson = do
          liftIO $ deletePersonFromDB db personId
          return NoContent

        changeNumber :: UpdatePersonDTO -> Handler Person
        changeNumber up = do
          res <- liftIO . updateNumberInDB db personId $ up

          case res of
            Nothing -> throwError err404 {errBody = "Person not found :("}
            Just p -> return p