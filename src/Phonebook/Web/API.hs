{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Phonebook.Web.API (API, server) where

import qualified Phonebook.Persons.Web as Persons
import qualified Phonebook.Users.Web as Users
import Phonebook.Web.AppM (AppM)
import Servant
import Servant.Auth.Server (JWTSettings)

type API = Users.API :<|> Persons.API

server :: JWTSettings -> ServerT API AppM
server jwts = Users.server jwts :<|> Persons.server
