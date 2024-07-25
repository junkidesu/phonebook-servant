{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web.Delete (Endpoint, handler) where

import Control.Monad (when)
import Data.Int (Int32)
import qualified Phonebook.Users.Database as Database
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  JWTAuth
    :> Summary "Delete user with the given ID"
    :> Capture "id" Int32
    :> Verb 'DELETE 204 '[JSON] NoContent

handler :: AuthResult Login.User -> Int32 -> AppM NoContent
handler (Authenticated lu) userId = do
  when (Login.id lu /= userId) $ throwError err401

  Database.deleteUser userId

  return NoContent
handler _ _ = throwError err401
