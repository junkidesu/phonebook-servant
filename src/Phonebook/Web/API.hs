{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Web.API (API, server) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Persons.Web as Persons
import qualified Phonebook.Users.Web as Users
import Servant
import Servant.Auth.Server (JWTSettings)

type API = Users.API :<|> Persons.API

server :: Pool Connection -> JWTSettings -> Server API
server conns jwts = Users.server conns jwts :<|> Persons.server conns
