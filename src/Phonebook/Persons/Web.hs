{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web (API, server) where

import qualified Phonebook.Persons.Web.All as All
import qualified Phonebook.Persons.Web.Avatar.Update as Avatar.Update
import qualified Phonebook.Persons.Web.Create as Create
import qualified Phonebook.Persons.Web.Delete as Delete
import qualified Phonebook.Persons.Web.Specific as Specific
import qualified Phonebook.Persons.Web.Update as Update
import Phonebook.Web.AppM (AppM)
import Servant

type API =
  "api"
    :> "persons"
    :> ( All.Endpoint
          :<|> Specific.Endpoint
          :<|> Create.Endpoint
          :<|> Delete.Endpoint
          :<|> Update.Endpoint
          :<|> Avatar.Update.Endpoint
       )

server :: ServerT API AppM
server =
  All.handler
    :<|> Specific.handler
    :<|> Create.handler
    :<|> Delete.handler
    :<|> Update.handler
    :<|> Avatar.Update.handler
