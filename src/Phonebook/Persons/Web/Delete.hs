{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Delete (Endpoint, handler) where

import Control.Monad (when)
import Data.Int (Int32)
import qualified Phonebook.Persons.Database as Database
import Phonebook.Users.Database.Table (UserT (_userId))
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.JWTAuth (JWTAuth)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  Summary "Remove the person with the given ID"
    :> JWTAuth
    :> Capture' '[Required, Description "ID of the person"] "id" Int32
    :> Verb 'DELETE 204 '[JSON] NoContent

handler :: AuthResult Login.User -> Int32 -> AppM NoContent
handler (Authenticated lu) personId = do
  mbPersonResult <- Database.personById personId

  case mbPersonResult of
    Nothing -> return NoContent
    Just (_, user) -> do
      when (_userId user /= Login.id lu) (throwError err401)

      Database.deletePerson personId

      return NoContent
handler _ _ = throwError err401
