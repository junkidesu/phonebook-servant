{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Persons (PersonsAPI, personsServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple
import Db.Operations
import Servant
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server (AuthResult (Authenticated))
import Types.AuthUser
import qualified Types.EditPerson as EP
import qualified Types.NewPerson as NP
import Types.Person

type JWTAuth = Auth '[JWT] AuthUser

type GetAllPersons = Summary "Get all persons in the app" :> Get '[JSON] [Person]
type AddPerson =
  Summary "Add user to the app"
    :> JWTAuth
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

personsServer :: Pool Connection -> Server PersonsAPI
personsServer conns =
  getAllPersons
    :<|> createPerson
    :<|> personOperations
 where
  getAllPersons :: Handler [Person]
  getAllPersons = liftIO . peopleInDB $ conns

  createPerson :: AuthResult AuthUser -> NP.NewPerson -> Handler Person
  createPerson (Authenticated au) np = do
    liftIO $ insertPerson conns (userId au) np
  createPerson _ _ = throwError err401{errBody = "Unauthenticated"}

  personOperations personId = getPerson :<|> deletePerson :<|> editPerson
   where
    getPerson :: Handler Person
    getPerson = do
      person <- liftIO $ personById conns personId

      case person of
        Nothing -> throwError err404{errBody = "Person not found :("}
        Just p -> return p

    deletePerson :: Handler NoContent
    deletePerson = do
      liftIO $ deletePersonFromDB conns personId
      return NoContent

    editPerson :: EP.EditPerson -> Handler Person
    editPerson up = do
      res <- liftIO . updateNumberInDB conns personId $ up

      case res of
        Nothing -> throwError err404{errBody = "Person not found :("}
        Just p -> return p
