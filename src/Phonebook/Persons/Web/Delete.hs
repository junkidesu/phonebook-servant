{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Delete (Endpoint, handler) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Persons.Database as Database
import Phonebook.Users.Database.Table (UserT (_userId))
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  Summary "Remove the person with the given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the person"] "id" Int32
    :> Verb 'DELETE 204 '[JSON] NoContent

handler :: Pool Connection -> AuthResult Login.User -> Int32 -> Handler NoContent
handler conns (Authenticated lu) personId = do
  mbPersonResult <- liftIO $ Database.personById conns personId

  case mbPersonResult of
    Nothing -> return NoContent
    Just (_, user) -> do
      when (_userId user /= Login.id lu) (throwError err401)

      liftIO $ Database.deletePerson conns personId

      return NoContent
handler _ _ _ = throwError err401
