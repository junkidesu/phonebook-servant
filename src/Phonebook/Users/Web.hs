{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web (API, server) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import qualified Phonebook.Users.Web.All as All
import qualified Phonebook.Users.Web.Login as Login
import qualified Phonebook.Users.Web.Register as Register
import Servant
import Servant.Auth.Server (JWTSettings)

type API = "api" :> "users" :> (All.Endpoint :<|> Register.Endpoint :<|> Login.Endpoint)

server :: Pool Connection -> JWTSettings -> Server API
server conns jwts =
  All.handler conns
    :<|> Register.handler conns
    :<|> Login.handler conns jwts
