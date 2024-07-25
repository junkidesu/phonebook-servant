{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Update (Endpoint, handler) where

import Control.Monad (when)
import Data.Int (Int32)
import Phonebook.Persons.Database (personById, toPersonType, updatePerson)
import qualified Phonebook.Persons.Person as Person
import qualified Phonebook.Persons.Person.Attributes as Attributes
import Phonebook.Users.Database.Table (UserT (_userId))
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.JWTAuth
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  Summary "Edit a person with the given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the person"] "id" Int32
    :> ReqBody '[JSON] Attributes.Edit
    :> Put '[JSON] Person.Person

handler :: AuthResult Login.User -> Int32 -> Attributes.Edit -> AppM Person.Person
handler (Authenticated lu) personId ep =
  do
    mbPersonResult <- personById personId

    case mbPersonResult of
      Nothing -> throwError err404
      Just (_, user) -> do
        when (_userId user /= Login.id lu) $ throwError err401

        toPersonType <$> updatePerson personId ep
handler _ _ _ = throwError err401
