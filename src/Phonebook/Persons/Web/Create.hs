{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Create (Endpoint, handler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Persons.Database as Database
import qualified Phonebook.Persons.Person as Person
import qualified Phonebook.Persons.Person.Attributes as Attributes
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.JWTAuth
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  Summary "Add user to the app"
    :> JWTAuth
    :> ReqBody' '[Required, Description "Name and number of the person to add"] '[JSON] Attributes.New
    :> PostCreated '[JSON] Person.Person

handler :: Pool Connection -> AuthResult Login.User -> Attributes.New -> Handler Person.Person
handler conns (Authenticated lu) np = do
  Database.toPersonType
    <$> liftIO
      ( Database.createPerson
          conns
          np
          $ Login.id lu
      )
handler _ _ _ = throwError err401