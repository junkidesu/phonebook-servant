{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API (PersonAPI, personsServer) where

import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DB
import DTO (NewPersonDTO)
import Database.SQLite.Simple
import Model
import Servant

type GetAllPersons = Get '[JSON] [Person]

type AddPerson = ReqBody '[JSON] NewPersonDTO :> PostCreated '[JSON] Person

type PersonAPI = "persons" :> (GetAllPersons :<|> AddPerson)

personsServer :: Connection -> Server PersonAPI
personsServer db = getAllPersons :<|> addPerson
  where
    getAllPersons :: Handler [Person]
    getAllPersons = liftIO . peopleInDB $ db

    addPerson :: NewPersonDTO -> Handler Person
    addPerson np = do
      res <- liftIO (try (insertPersonInDB db np) :: IO (Either SQLError Person))

      case res of
        Left _ -> throwError err400 {errBody = "Couldn't add new person :("}
        Right p -> return p