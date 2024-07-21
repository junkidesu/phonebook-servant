{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web.Delete (Endpoint, handler) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Users.Database as Database
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  JWTAuth
    :> Summary "Delete user with the given ID"
    :> Capture "id" Int32
    :> Verb 'DELETE 204 '[JSON] NoContent

handler :: Pool Connection -> AuthResult Login.User -> Int32 -> Handler NoContent
handler conns (Authenticated lu) userId = do
  when (Login.id lu /= userId) $ throwError err401

  liftIO $ Database.deleteUser conns userId

  return NoContent
handler _ _ _ = throwError err401
