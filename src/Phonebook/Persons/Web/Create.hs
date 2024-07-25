{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.Create (Endpoint, handler) where

import qualified Phonebook.Persons.Database as Database
import qualified Phonebook.Persons.Person as Person
import qualified Phonebook.Persons.Person.Attributes as Attributes
import qualified Phonebook.Users.Web.Login.User as Login
import Phonebook.Web.AppM (AppM)
import Phonebook.Web.JWTAuth
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

type Endpoint =
  Summary "Add user to the app"
    :> JWTAuth
    :> ReqBody' '[Required, Description "Name and number of the person to add"] '[JSON] Attributes.New
    :> PostCreated '[JSON] Person.Person

handler :: AuthResult Login.User -> Attributes.New -> AppM Person.Person
handler (Authenticated lu) np = do
  Database.toPersonType
    <$> Database.createPerson np (Login.id lu)
handler _ _ = throwError err401
