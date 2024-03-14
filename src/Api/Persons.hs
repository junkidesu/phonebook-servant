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
import qualified Types.AuthUser as AU
import qualified Types.EditPerson as EP
import qualified Types.NewPerson as NP
import qualified Types.Person as P
import qualified Types.User as U

type JWTAuth = Auth '[JWT] AU.AuthUser

type GetAllPersons = Summary "Get all persons in the app" :> Get '[JSON] [P.Person]
type AddPerson =
  Summary "Add user to the app"
    :> JWTAuth
    :> ReqBody' '[Required, Description "Name and number of the person to add"] '[JSON] NP.NewPerson
    :> PostCreated '[JSON] P.Person

type GetPersonById =
  Summary "Get person by ID"
    :> Capture' '[Required, Description "ID of the person"] "id" Int
    :> Get '[JSON] P.Person

type DeletePerson =
  Summary "Remove the person with the given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the person"] "id" Int
    :> Verb 'DELETE 204 '[JSON] NoContent

type EditPerson =
  Summary "Edit a person with the given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the person"] "id" Int
    :> ReqBody '[JSON] EP.EditPerson
    :> Put '[JSON] P.Person

type PersonsAPI =
  "persons"
    :> ( GetAllPersons
          :<|> AddPerson
          :<|> GetPersonById
          :<|> DeletePerson
          :<|> EditPerson
       )

userIsAuthor :: AU.AuthUser -> P.Person -> Bool
userIsAuthor au p = AU.id au == (U.id . P.author $ p)

personsServer :: Pool Connection -> Server PersonsAPI
personsServer conns =
  getAllPersons
    :<|> createPerson
    :<|> getPerson
    :<|> deletePerson
    :<|> editPerson
 where
  getAllPersons :: Handler [P.Person]
  getAllPersons = liftIO . peopleInDB $ conns

  createPerson :: AuthResult AU.AuthUser -> NP.NewPerson -> Handler P.Person
  createPerson (Authenticated au) np = do
    liftIO $ insertPerson conns (AU.id au) np
  createPerson _ _ = throwError err401{errBody = "Unauthenticated"}

  getPerson :: Int -> Handler P.Person
  getPerson personId = do
    person <- liftIO $ personById conns personId

    case person of
      Nothing -> throwError err404{errBody = "Person not found :("}
      Just p -> return p

  deletePerson :: AuthResult AU.AuthUser -> Int -> Handler NoContent
  deletePerson (Authenticated au) personId = do
    mbPerson <- liftIO $ personById conns personId

    case mbPerson of
      Nothing -> throwError err404
      Just person -> do
        if not $ userIsAuthor au person
          then throwError err401
          else do
            liftIO (deletePersonFromDB conns personId)
            return NoContent
  deletePerson _ _ = throwError err401

  editPerson :: AuthResult AU.AuthUser -> Int -> EP.EditPerson -> Handler P.Person
  editPerson (Authenticated au) personId up = do
    mbPerson <- liftIO . personById conns $ personId

    case mbPerson of
      Nothing -> throwError err404
      Just person -> do
        if not $ userIsAuthor au person
          then throwError err401
          else liftIO $ updateNumberInDB conns personId up
  editPerson _ _ _ = throwError err401
