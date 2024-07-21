{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Update (Endpoint, handler) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Phonebook.Persons.Database (personById, toPersonType, updatePerson)
import qualified Phonebook.Persons.Person as Person
import qualified Phonebook.Persons.Person.Attributes as Attributes
import Phonebook.Users.Database.Table (UserT (_userId))
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.JWTAuth
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  Summary "Edit a person with the given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the person"] "id" Int32
    :> ReqBody '[JSON] Attributes.Edit
    :> Put '[JSON] Person.Person

handler :: Pool Connection -> AuthResult Login.User -> Int32 -> Attributes.Edit -> Handler Person.Person
handler conns (Authenticated lu) personId ep =
  do
    mbPersonResult <- liftIO $ personById conns personId

    case mbPersonResult of
      Nothing -> throwError err404
      Just (_, user) -> do
        when (_userId user /= Login.id lu) $ throwError err401

        liftIO (toPersonType <$> updatePerson conns personId ep)
handler _ _ _ _ = throwError err401
