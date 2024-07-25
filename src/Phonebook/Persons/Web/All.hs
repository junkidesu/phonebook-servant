{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.All (Endpoint, handler) where

import Phonebook.Persons.Database (allPersons, toPersonType)
import qualified Phonebook.Persons.Person as Person
import Phonebook.Web.AppM (AppM)
import Servant

type Endpoint = Summary "Get all persons in the app" :> Get '[JSON] [Person.Person]

handler :: AppM [Person.Person]
handler = do
  map toPersonType <$> allPersons
