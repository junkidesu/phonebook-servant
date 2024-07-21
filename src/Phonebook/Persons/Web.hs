{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Persons.Web (API, server) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Persons.Web.All as All
import qualified Phonebook.Persons.Web.Specific as Specific
import Servant

type API =
  "api"
    :> "persons"
    :> ( All.Endpoint
          :<|> Specific.Endpoint
       )

server :: Pool Connection -> Server API
server conns = All.handler conns :<|> Specific.handler conns
