{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web.All where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Phonebook.Persons.Database (allPersons, toPersonType)
import qualified Phonebook.Persons.Person as Person
import Servant

type Endpoint = Summary "Get all persons in the app" :> Get '[JSON] [Person.Person]

handler :: Pool Connection -> Handler [Person.Person]
handler conns = liftIO $ map toPersonType <$> allPersons conns
