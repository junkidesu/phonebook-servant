{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Persons (PersonsAPI, personsServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Database.PostgreSQL.Simple
import Db.Model.Person
import Db.Operations
import qualified Dto.EditPerson as EP
import qualified Dto.NewPerson as NP
import Servant

type GetAllPersons = Summary "Get all persons in the app" :> Get '[JSON] [Person]
type AddPerson =
  Summary "Add user to the app"
    :> Capture "userId" Int
    :> ReqBody' '[Required, Description "Name and number of the person to add"] '[JSON] NP.NewPerson
    :> PostCreated '[JSON] Person
type GetPerson = Summary "Get person by ID" :> Get '[JSON] Person
type DeletePerson = Summary "Remove the person with the given ID" :> DeleteNoContent
type EditPerson = Summary "Edit a person with the given ID" :> ReqBody '[JSON] EP.EditPerson :> Put '[JSON] Person
type PersonOperations =
  Capture' '[Description "ID of the person"] "id" Int
    :> ( GetPerson
          :<|> DeletePerson
          :<|> EditPerson
       )

type PersonsAPI =
  "persons"
    :> ( GetAllPersons
          :<|> AddPerson
          :<|> PersonOperations
       )

personsServer :: Connection -> Server PersonsAPI
personsServer conn =
  getAllPersons
    :<|> createPerson
    :<|> personOperations
 where
  getAllPersons :: Handler [Person]
  getAllPersons = liftIO . peopleInDB $ conn

  createPerson :: Int -> NP.NewPerson -> Handler Person
  createPerson userId np = do
    liftIO $ insertPerson conn userId np

  personOperations personId = getPerson :<|> deletePerson :<|> editPerson
   where
    getPerson :: Handler Person
    getPerson = do
      person <- liftIO $ personById conn personId

      case person of
        Nothing -> throwError err404{errBody = "Person not found :("}
        Just p -> return p

    deletePerson :: Handler NoContent
    deletePerson = do
      liftIO $ deletePersonFromDB conn personId
      return NoContent

    editPerson :: EP.EditPerson -> Handler Person
    editPerson up = do
      res <- liftIO . updateNumberInDB conn personId $ up

      case res of
        Nothing -> throwError err404{errBody = "Person not found :("}
        Just p -> return p
