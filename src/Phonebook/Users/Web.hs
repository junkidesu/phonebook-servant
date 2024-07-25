{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Users.Web (API, server) where

import qualified Phonebook.Users.Web.All as All
import qualified Phonebook.Users.Web.Delete as Delete
import qualified Phonebook.Users.Web.Login as Login
import qualified Phonebook.Users.Web.Register as Register
import Phonebook.Web.AppM (AppM)
import Servant
import Servant.Auth.Server (JWTSettings)

type API =
  "api"
    :> "users"
    :> ( All.Endpoint
          :<|> Register.Endpoint
          :<|> Delete.Endpoint
          :<|> Login.Endpoint
       )

server :: JWTSettings -> ServerT API AppM
server jwts =
  All.handler
    :<|> Register.handler
    :<|> Delete.handler
    :<|> Login.handler jwts
