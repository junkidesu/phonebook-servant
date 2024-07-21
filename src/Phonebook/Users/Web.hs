{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web (API, server) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Users.Web.All as All
import qualified Phonebook.Users.Web.Delete as Delete
import qualified Phonebook.Users.Web.Login as Login
import qualified Phonebook.Users.Web.Register as Register
import Servant
import Servant.Auth.Server (JWTSettings)

type API = "api" :> "users" :> (All.Endpoint :<|> Register.Endpoint :<|> Delete.Endpoint :<|> Login.Endpoint)

server :: Pool Connection -> JWTSettings -> Server API
server conns jwts =
  All.handler conns
    :<|> Register.handler conns
    :<|> Delete.handler conns
    :<|> Login.handler conns jwts
